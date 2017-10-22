module arff;

import std.algorithm;
import std.array;
import std.conv;
import std.csv;
import std.exception;
import std.math;
import std.range;
import std.stdio;
import std.string;
import std.uni;

/**
	Indicates the type of an attribute.
*/
enum AttributeType
{
    numeric,
    nominal
}

/**
	Contains metadata relating to an attribute in the ARFF file.
*/
struct Attribute
{
    public
    {
		/**
			Constructs an $(D Attribute) representing a numeric attribute with the given name.

			Params:
				name = The name of the attribute.
		*/
        this(string name)
        {
            mName = name;
            mType = AttributeType.numeric;
        }

		/**
			Constructs an $(D Attribute) representing a nominal attribute with the given name and possible values.

			Params:
				name = The name of the attribute.
				categories = The possible values that this attribute can take.
		*/
        this(string name, string[] categories)
        {
            mName = name;
            mType = AttributeType.nominal;
            mCategories = categories.dup;
        }

		/**
			For a nominal attribute, returns a floating point value used to represent the given category internally.
		*/
        float stringToFloat(string category) const
        {
            enforce(mType == AttributeType.nominal,
                "Cannot perform stringToFloat because '" ~ name ~ "' is not a nominal attribute");

            auto ind = mCategories.countUntil(category);

            enforce(ind != -1, "Unknown nominal value '" ~ category ~ "' for attribute '" ~ mName ~ "'");

            return cast(float)ind;
        }

        @property string name() const
        {
            return mName;
        }

        @property AttributeType type() const
        {
            return mType;
        }

        @property const(string[]) categories() const
        {
            return mCategories;
        }
    }

    private
    {
        string mName;
        AttributeType mType;
        string[] mCategories;
    }
}

/**
	Stores a collection of instances loaded from an ARFF file.
*/
struct ARFF
{
    public
    {
        this(string name, Attribute[] attribs, float[][] vals, uint lbls)
        {
            mName = name;

            mAttributes = attribs.array;

            mValues = vals
                     .map!array
                     .array;

            mLabels = lbls;
        }

        float[] obscureLabels(float[] inst)
        {
            auto newInst = inst.dup;
            newInst[$ - mLabels .. $] = float.nan;

            return newInst;
        }

        @property string name() const
        {
            return mName;
        }

        @property Attribute[] attributes()
        {
            return mAttributes;
        }

        @property uint labels()
        {
            return mLabels;
        }

        @property uint features()
        {
            return cast(uint)mAttributes.length - mLabels;
        }

        @property float[][] values()
        {
            return mValues;
        }
    }

    private
    {
        string mName;
        Attribute[] mAttributes;
        uint mLabels;
        float[][] mValues;
    }
}

private bool consume(R)(ref R line, string kw)
{
    if(line.map!toLower.startsWith(kw.map!toLower))
    {
        line.popFrontN(kw.length);

        return true;
    }
    else
    {
        return false;
    }
}

private string consumeWord(R)(ref R line)
{
    auto ret = line.until!isWhite.to!string;
    line.popFrontN(ret.length);

    return ret;
}

private void skip(R)(ref R line)
{
    while(!line.empty && line.front.isWhite)
    {
        line.popFront;
    }
}

ARFF loadARFF(string path)
{
    auto f = File(path, "r");
    int numLabels = int.min;
    bool swapLabels;
    bool dataMode = false;
    string name;
    string[] attribNames;
    Attribute[] attribs;
    float[][] vals;

    auto inputRange = f.byLineCopy.map!strip;

    foreach(l; inputRange)
    {
        if(!dataMode && l.length > 0 && l.front == '@')
        {
            if(l.consume("@relation"))
            {
                enforce(name == "", "Relation cannot have multiple @relation statements");

                enforce(attribs.length == 0,
                    "The @relation statement must occur before any @attribute statements");

                name = l.strip;

                enforce(name != "", "The relation must have a name");

                if(name.front == name.back && name.length > 1 && name.front == '\'' || name.front == '"')
                {
                    name = name[1 .. $ - 1];
                }

                enforce(name != "", "The relation must have a name");

                import std.getopt;
                auto args = name.splitter().array;
                getopt(args, config.passThrough, "C", &numLabels);

                swapLabels = numLabels > 0;
                numLabels = numLabels == int.min ? 1 : abs(numLabels);
            }
            else if(l.consume("@attribute"))
            {
                enforce(name != "", "The @relation statement must occur before any @attibute statements");

                l.skip();
                auto attName = l.consumeWord;
                l.skip();

                auto attSpec = l.strip;

                if(attSpec.asLowerCase.equal("numeric") || attSpec.asLowerCase.equal("real"))
                {
                    attribs ~= Attribute(attName);
                }
                else if(attSpec.front == '{' && attSpec.back == '}')
                {
                    auto cats = attSpec[1 .. $ - 1].csvReader!string;
                    attribs ~= Attribute(attName, cats.front.map!(strip).array());
                }
                else
                {
                    throw new Exception("Unsupported attribute type\n" ~ l ~ "\n" ~ attName ~ "\n" ~ attSpec);
                }
            }
            else if(l.consume("@data"))
            {
                dataMode = true;
            }
        }
        else if(dataMode && l.length > 0)
        {
            float[] instVals = new float[attribs.length];
            instVals[] = 0.0f;

            if(l[0] == '{' && l[$ - 1] == '}')
            {
                foreach(s; l[1 .. $ - 1].splitter(','))
                {
                    auto kv = s.splitter();
                    auto key = kv.front.to!size_t;
                    kv.popFront;
                    auto val = kv.front;

                    if(attribs[key].type == AttributeType.nominal)
                    {
                        instVals[key] = attribs[key].stringToFloat(val);
                    }
                    else
                    {
                        instVals[key] = val.to!float;
                    }
                }
            }
            else
            {
                instVals = zip(attribs, l.splitter(',').map!(x => x.strip()))
                          .map!(x => x[0].type == AttributeType.numeric ? x[1].to!float : x[0].stringToFloat(x[1]))
                          .array();
            }

            if(swapLabels)
            {
                instVals = instVals[numLabels .. $] ~ instVals[0 .. numLabels];
                attribs = attribs[numLabels .. $] ~ attribs[0 .. numLabels];
            }

            vals ~= instVals;
        }
    }
    
    return ARFF(name, attribs, vals, cast(uint)numLabels);
}