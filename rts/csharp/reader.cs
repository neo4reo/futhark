    internal Stack<char> LookaheadBuffer = new Stack<char>();
    void ResetLookahead(){LookaheadBuffer.Clear();}

    char? GetChar(Stream f)
    {
        char c;
        if (LookaheadBuffer.Count == 0)
        {
            c = (char) f.ReadByte();
        }
        else
        {
            c = LookaheadBuffer.Pop();
        }

        return c;
    }

    char[] GetChars(Stream f, int n)
    {
        return Enumerable.Range(0, n).Select(_ => GetChar(f).Value).ToArray();
    }

    void UngetChar(char c)
    {
        LookaheadBuffer.Push(c);
    }

    char PeekChar(Stream f)
    {
        var c = GetChar(f);
        UngetChar(c.Value);
        return c.Value;
    }

    void SkipSpaces(Stream f)
    {
        var c = GetChar(f);
        while (c.HasValue){
            if (char.IsWhiteSpace(c.Value))
            {
                c = GetChar(f);
            }
            else if (c == '-')
            {
                if (PeekChar(f) == '-')
                {
                    while (c.Value != '\n')
                    {
                        c = GetChar(f);
                    }
                }
                else
                {
                    break;
                }
            }
            else
            {
                break;
            }
        }
    
        if (c.HasValue)
        {
            UngetChar(c.Value);
        }
    }

    bool ParseSpecificChar(Stream f, char c)
    {
        var got = GetChar(f);
        if (got.Value != c)
        {
            UngetChar(got.Value);
            throw new Exception("ValueError");
        }
        return true;
    }

    bool ParseSpecificString(Stream f, string str)
    {
        foreach (var c in str.ToCharArray())
        {
            ParseSpecificChar(f, c);
        }

        return true;
    }


    string Optional(Func<Stream, string> p, Stream f)
    {
        string res = null;
        try
        {
            res = p(f);
        }
        catch (Exception)
        {
        }

        return res;
    }
    
    bool Optional(Func<Stream, char, bool> p, Stream f, char c)
    {
        try
        {
            return p(f,c);
        }
        catch (Exception)
        {
        }

        return false;
    }

    bool OptionalSpecificString(Stream f, string s)
    {
        var c = PeekChar(f);
        if (c == s[0])
        {
            return ParseSpecificString(f, s);
        }
        return false;
    }
        
        
        List<string> sepBy(Func<Stream, string> p, Func<Stream, string> sep, Stream arg)
        {
            var elems = new List<string>();
            var x = Optional(p, arg);
            if (!string.IsNullOrWhiteSpace(x))
            {
                elems.Add(x);
                while (!string.IsNullOrWhiteSpace(Optional(sep, arg)))
                {
                    var y = Optional(p, arg);
                    elems.Add(y);
                } 
            }
            return elems;
        }

    string ParseHexInt(Stream f)
    {
        var s = "";
        var c = GetChar(f);
        while (c.HasValue)
        {
            if (Uri.IsHexDigit(c.Value))
            {
                s += c.Value;
                c = GetChar(f);
            }
            else if (c == '_')
            {
                c = GetChar(f);
            }
            else
            {
                UngetChar(c.Value);
                break;
            }
        }

        return Convert.ToString(Convert.ToUInt32(s, 16));
    }

    string ParseInt(Stream f)
    {
        var s = "";
        var c = GetChar(f);
        if (c.Value == '0' && "xX".Contains(PeekChar(f)))
        {
            GetChar(f);
            s += ParseHexInt(f);
        }
        else
        {
            while (c.HasValue)
            {
                if (char.IsDigit(c.Value))
                {
                    s += c.Value;                    
                    c = GetChar(f);
                }else if (c == '_')
                {
                    c = GetChar(f);
                }
                else
                {
                    UngetChar(c.Value);
                    break;
                }
            }
            
        }

        if (s.Length == 0)
        {
            throw new Exception("ValueError");
        }

        return s;
    }

    string ParseIntSigned(Stream f)
    {
        var s = "";
        var c = GetChar(f);
        if (c.Value == '-' && char.IsDigit(PeekChar(f)))
        {
            return c + ParseInt(f);
        }
        else
        {
            if (c.Value != '+')
            {
                UngetChar(c.Value);
            }

            return ParseInt(f);
        }
    }

    string ReadStrComma(Stream f)
    {
        SkipSpaces(f);
        ParseSpecificChar(f, ',');
        return ",";
    }

    int ReadStrInt(Stream f, string s)
    {
        SkipSpaces(f);
        var x = Convert.ToInt32(ParseIntSigned(f));
        OptionalSpecificString(f, s);
        return x;
    }
    
    int ReadStrUint(Stream f, string s)
    {
        SkipSpaces(f);
        var x = Convert.ToInt32(ParseInt(f));
        OptionalSpecificString(f, s);
        return x;
    }

    int ReadStrI8(Stream f){return ReadStrInt(f, "i8");}
    int ReadStrI16(Stream f){return ReadStrInt(f, "i16");}
    int ReadStrI32(Stream f){return ReadStrInt(f, "i32");}
    int ReadStrI64(Stream f){return ReadStrInt(f, "i64");}
    int ReadStrU8(Stream f){return ReadStrInt(f, "u8");}
    int ReadStrU16(Stream f){return ReadStrInt(f, "u16");}
    int ReadStrU32(Stream f){return ReadStrInt(f, "u32");}
    int ReadStrU64(Stream f){return ReadStrInt(f, "u64");}

    char ReadChar(Stream f)
    {
        SkipSpaces(f);
        ParseSpecificChar(f, '\'');
        var c = GetChar(f);
        ParseSpecificChar(f, '\'');
        return c.Value;
    }

    float ReadStrHexFloat(Stream f, char sign)
    {
        var int_part = ParseHexInt(f);
        ParseSpecificChar(f, '.');
        var frac_part = ParseHexInt(f);
        ParseSpecificChar(f, 'p');
        var exponent = ParseHexInt(f);

        var int_val = Convert.ToInt32(int_part, 16);
        var frac_val = Convert.ToSingle(Convert.ToInt32(frac_part, 16)) / Math.Pow(16, frac_part.Length);
        var exp_val = Convert.ToInt32(exponent);

        var total_val = (int_val + frac_val) * Math.Pow(2, exp_val);
        if (sign == '-')
        {
            total_val = -1 * total_val;
        }

        return Convert.ToSingle(total_val);
    }

    float ReadStrDecimal(Stream f)
    {
        SkipSpaces(f);
        var c = GetChar(f);
        char sign;
        if (c.Value == '-')
        {
            sign = '-';
        }
        else
        {
            UngetChar(c.Value);
            sign = '+';
        }
        
        // Check for hexadecimal float
        c = GetChar(f);
        if (c.Value == '0' && "xX".Contains(PeekChar(f)))
        {
            GetChar(f);
            return ReadStrHexFloat(f, sign);
        }
        else
        {
            UngetChar(c.Value);
        }

        var bef = Optional(ParseInt, f);
        var aft = "";
        if (string.IsNullOrEmpty(bef))
        {
            bef = "0";
            ParseSpecificChar(f, '.');
            aft = ParseInt(f);
        }else if (Optional(ParseSpecificChar, f, '.'))
        {
            aft = ParseInt(f);
        }
        else
        {
            aft = "0";
        }

        var expt = "";
        if (Optional(ParseSpecificChar, f, 'E') ||
            Optional(ParseSpecificChar, f, 'e'))
        {
            expt = ParseIntSigned(f);
        }
        else
        {
            expt = "0";
        }

        return Convert.ToSingle(sign + bef + "." + aft + "E" + expt);
    }

    float ReadStrF32(Stream f)
    {
        var x = ReadStrDecimal(f);
        OptionalSpecificString(f, "f32");
        return x;
    }
    
    float ReadStrF64(Stream f)
    {
        var x = ReadStrDecimal(f);
        OptionalSpecificString(f, "f64");
        return x;
    }

    bool ReadStrBool(Stream f)
    {
        SkipSpaces(f);
        if (PeekChar(f) == 't')
        {
            ParseSpecificString(f, "true");
            return true;
        }

        if (PeekChar(f) == 'f')
        {
            ParseSpecificString(f, "false");
            return false;
        }

        throw new Exception("ValueError");
    }

    sbyte read_i8(Stream f)
    {
        return (sbyte) ReadStrI8(f);
    }
    short read_i16(Stream f)
    {
        return (short) ReadStrI16(f);
    }
    int read_i32(Stream f)
    {
        return ReadStrI32(f);
    }
    long read_i64(Stream f)
    {
        return ReadStrI64(f);
    }
    
    byte read_u8(Stream f)
    {
        return (byte) ReadStrU8(f);
    }
    ushort read_u16(Stream f)
    {
        return (ushort) ReadStrU16(f);
    }
    uint read_u32(Stream f)
    {
        return (uint) ReadStrU32(f);
    }
    ulong read_u64(Stream f)
    {
        return (ulong) ReadStrU64(f);
    }
    
    bool read_bool(Stream f)
    {
        return ReadStrBool(f);
    }
    
    float read_f32(Stream f)
    {
        return ReadStrDecimal(f);
    }
    double read_f64(Stream f)
    {
        return ReadStrDecimal(f);
    }


    Stream getStream()
    {
        return Console.OpenStandardInput();
    }
