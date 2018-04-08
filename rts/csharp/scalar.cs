// Libraries below are imported in SequentialCSharp.hs
// using System;
// using static System.Convert;
// using static System.Math;

// Scalar functions.
static sbyte signed(byte x){ return Convert.ToSByte(x);}
static short signed(ushort x){ return Convert.ToInt16(x);}
static int signed(uint x){ return Convert.ToInt32(x);}
static long signed(ulong x){ return Convert.ToInt64(x);}

static byte unsigned(sbyte x){ return Convert.ToByte(x);}
static ushort unsigned(short x){ return Convert.ToUInt16(x);}
static uint unsigned(int x){ return Convert.ToUInt32(x);}
static ulong unsigned(long x){ return Convert.ToUInt64(x);}

static sbyte shl8(sbyte x, sbyte y){ return Convert.ToSByte(x << y); }
static short shl16(short x, short y){ return Convert.ToInt16(x << y); }
static int shl32(int x, int y){ return x << y; }
static long shl64(long x, long y){ return x << Convert.ToInt32(y); }

static sbyte ashr8(sbyte x, sbyte y){ return Convert.ToSByte(x >> y); }
static short ashr16(short x, short y){ return Convert.ToSByte(x >> y); }
static int ashr32(int x, int y){ return x >> y; }
static long ashr64(long x, long y){ return x >> Convert.ToInt32(y); }

static sbyte sdiv8(sbyte x, sbyte y){ return Convert.ToSByte(x / y); }
static short sdiv16(short x, short y){ return Convert.ToSByte(x / y); }
static int sdiv32(int x, int y){ return x / y; }
static long sdiv64(long x, long y){ return x / y; }

static sbyte smod8(sbyte x, sbyte y){ return Convert.ToSByte(x % y); }
static short smod16(short x, short y){ return Convert.ToSByte(x % y); }
static int smod32(int x, int y){ return x % y; }
static long smod64(long x, long y){ return x % y; }

static sbyte udiv8(sbyte x, sbyte y){ return Convert.ToSByte(unsigned(x) / unsigned(y)); }
static short udiv16(short x, short y){ return Convert.ToInt16(unsigned(x) / unsigned(y)); }
static int udiv32(int x, int y){ return Convert.ToInt32(unsigned(x) / unsigned(y)); }
static long udiv64(long x, long y){ return Convert.ToInt64(unsigned(x) / unsigned(y)); }

static sbyte umod8(sbyte x, sbyte y){ return Convert.ToSByte(unsigned(x) % unsigned(y)); }
static short umod16(short x, short y){ return Convert.ToInt16(unsigned(x) % unsigned(y)); }
static int umod32(int x, int y){ return Convert.ToInt32(unsigned(x) % unsigned(y)); }
static long umod64(long x, long y){ return Convert.ToInt64(unsigned(x) % unsigned(y)); }

static sbyte squot8(sbyte x, sbyte y){ return Convert.ToSByte(ToSingle(x) / ToSingle(y)); }
static short squot16(short x, short y){ return Convert.ToInt16(ToSingle(x) / ToSingle(y)); }
static int squot32(int x, int y){ return Convert.ToInt32(ToSingle(x) / ToSingle(y)); }
static long squot64(long x, long y){ return Convert.ToInt64(ToSingle(x) / ToSingle(y)); }

// static Maybe change srem, it calls np.fmod originally so i dont know
static sbyte srem8(sbyte x, sbyte y){ return sdiv8(x,y);}
static short srem16(short x, short y){ return sdiv16(x,y);}
static int srem32(int x, int y){ return sdiv32(x,y);}
static long srem64(long x, long y){ return sdiv64(x,y);}

static sbyte smin8(sbyte x, sbyte y){ return Math.Min(x,y);}
static short smin16(short x, short y){ return Math.Min(x,y);}
static int smin32(int x, int y){ return Math.Min(x,y);}
static long smin64(long x, long y){ return Math.Min(x,y);}

static sbyte smax8(sbyte x, sbyte y){ return Math.Max(x,y);}
static short smax16(short x, short y){ return Math.Max(x,y);}
static int smax32(int x, int y){ return Math.Max(x,y);}
static long smax64(long x, long y){ return Math.Max(x,y);}

static sbyte umin8(sbyte x, sbyte y){ return signed(Math.Min(unsigned(x),unsigned(y)));}
static short umin16(short x, short y){ return signed(Math.Min(unsigned(x),unsigned(y)));}
static int umin32(int x, int y){ return signed(Math.Min(unsigned(x),unsigned(y)));}
static long umin64(long x, long y){ return signed(Math.Min(unsigned(x),unsigned(y)));}

static sbyte umax8(sbyte x, sbyte y){ return signed(Math.Max(unsigned(x),unsigned(y)));}
static short umax16(short x, short y){ return signed(Math.Max(unsigned(x),unsigned(y)));}
static int umax32(int x, int y){ return signed(Math.Max(unsigned(x),unsigned(y)));}
static long umax64(long x, long y){ return signed(Math.Max(unsigned(x),unsigned(y)));}

static float fmin32(float x, float y){ return Math.Min(x,y);}
static double fmin64(double x, double y){ return Math.Min(x,y);}
static float fmax32(float x, float y){ return Math.Max(x,y);}
static double fmax64(double x, double y){ return Math.Max(x,y);}

static sbyte pow8(sbyte x, sbyte y){ return Convert.ToSByte(Math.Pow(x,y));}
static short pow16(short x, short y){ return Convert.ToInt16(Math.Pow(x,y));}
static int pow32(int x, int y){ return Convert.ToInt32(Math.Pow(x,y));}
static long pow64(long x, long y){ return Convert.ToInt64(Math.Pow(x,y));}
static float fpow32(float x, float y){ return Convert.ToSingle(Math.Pow(x,y));}
static double fpow64(double x, double y){ return Convert.ToDouble(Math.Pow(x,y));}

static bool sle8(sbyte x, sbyte y){ return x <= y ;}
static bool sle16(short x, short y){ return x <= y ;}
static bool sle32(int x, int y){ return x <= y ;}
static bool sle64(long x, long y){ return x <= y ;}

static bool slt8(sbyte x, sbyte y){ return x < y ;}
static bool slt16(short x, short y){ return x < y ;}
static bool slt32(int x, int y){ return x < y ;}
static bool slt64(long x, long y){ return x < y ;}

static bool ule8(sbyte x, sbyte y){ return unsigned(x) <= unsigned(y) ;}
static bool ule16(short x, short y){ return unsigned(x) <= unsigned(y) ;}
static bool ule32(int x, int y){ return unsigned(x) <= unsigned(y) ;}
static bool ule64(long x, long y){ return unsigned(x) <= unsigned(y) ;}

static bool ult8(sbyte x, sbyte y){ return unsigned(x) < unsigned(y) ;}
static bool ult16(short x, short y){ return unsigned(x) < unsigned(y) ;}
static bool ult32(int x, int y){ return unsigned(x) < unsigned(y) ;}
static bool ult64(long x, long y){ return unsigned(x) < unsigned(y) ;}

static sbyte lshr8(sbyte x, sbyte y){ return Convert.ToSByte(Convert.ToByte(x) >> Convert.ToByte(y));}
static short lshr16(short x, short y){ return Convert.ToInt16(Convert.ToUInt16(x) >> Convert.ToUInt16(y));}
static int lshr32(int x, int y){ return Convert.ToInt32(Convert.ToUInt32(x) >> Convert.ToInt32(y));}
static long lshr64(long x, long y){ return Convert.ToInt64(Convert.ToUInt64(x) >> Convert.ToInt32(y));}

static sbyte sext_i8_i8(sbyte x){return Convert.ToSByte(x);}
static sbyte sext_i16_i8(short x){return Convert.ToSByte(x);}
static sbyte sext_i32_i8(int x){return Convert.ToSByte(x);}
static sbyte sext_i64_i8(long x){return Convert.ToSByte(x);}

static short sext_i8_i16(sbyte x){return Convert.ToInt16(x);}
static short sext_i16_i16(short x){return Convert.ToInt16(x);}
static short sext_i32_i16(int x){return Convert.ToInt16(x);}
static short sext_i64_i16(long x){return Convert.ToInt16(x);}

static int sext_i8_i32(sbyte x){return Convert.ToInt32(x);}
static int sext_i16_i32(short x){return Convert.ToInt32(x);}
static int sext_i32_i32(int x){return Convert.ToInt32(x);}
static int sext_i64_i32(long x){return Convert.ToInt32(x);}

static long sext_i8_i64(sbyte x){return Convert.ToInt64(x);}
static long sext_i16_i64(short x){return Convert.ToInt64(x);}
static long sext_i32_i64(int x){return Convert.ToInt64(x);}
static long sext_i64_i64(long x){return Convert.ToInt64(x);}


static sbyte zext_i8_i8(sbyte x){return Convert.ToSByte(Convert.ToByte(x));}
static sbyte zext_i16_i8(short x){return Convert.ToSByte(Convert.ToByte(x));}
static sbyte zext_i32_i8(int x){return Convert.ToSByte(Convert.ToByte(x));}
static sbyte zext_i64_i8(long x){return Convert.ToSByte(Convert.ToByte(x));}

static short zext_i8_i16(sbyte x){return Convert.ToInt16(Convert.ToUInt16(x));}
static short zext_i16_i16(short x){return Convert.ToInt16(Convert.ToUInt16(x));}
static short zext_i32_i16(int x){return Convert.ToInt16(Convert.ToUInt16(x));}
static short zext_i64_i16(long x){return Convert.ToInt16(Convert.ToUInt16(x));}

static int zext_i8_i32(sbyte x){return Convert.ToInt32(Convert.ToUInt32(x));}
static int zext_i16_i32(short x){return Convert.ToInt32(Convert.ToUInt32(x));}
static int zext_i32_i32(int x){return Convert.ToInt32(Convert.ToUInt32(x));}
static int zext_i64_i32(long x){return Convert.ToInt32(Convert.ToUInt32(x));}

static long zext_i8_i64(sbyte x){return Convert.ToInt64(Convert.ToUInt64(x));}
static long zext_i16_i64(short x){return Convert.ToInt64(Convert.ToUInt64(x));}
static long zext_i32_i64(int x){return Convert.ToInt64(Convert.ToUInt64(x));}
static long zext_i64_i64(long x){return Convert.ToInt64(Convert.ToUInt64(x));}

static int ssignum8(sbyte x){return Math.Sign(x);}
static int ssignum16(short x){return Math.Sign(x);}
static int ssignum32(int x){return Math.Sign(x);}
static int ssignum64(long x){return Math.Sign(x);}

static int usignum8(byte x){return x < 0 ? ssignum8(Convert.ToSByte(-x)) : ssignum8(Convert.ToSByte(x));}
static int usignum16(ushort x){return x < 0 ? ssignum16(Convert.ToInt16(-x)) : ssignum16(Convert.ToInt16(x));}
static int usignum32(uint x){return x < 0 ? ssignum32(Convert.ToInt32(-x)) : ssignum32(Convert.ToInt32(x));}
static int usignum64(ulong x){return x < 0 ? ssignum64(Convert.ToInt64(0-x)) : ssignum64(Convert.ToInt64(x));}

static int ssignum(sbyte x){return Math.Sign(x);}
static int ssignum(short x){return Math.Sign(x);}
static int ssignum(int x){return Math.Sign(x);}
static int ssignum(long x){return Math.Sign(x);}

static int usignum(byte x){return x < 0 ? ssignum8(Convert.ToSByte(-x)) : ssignum8(Convert.ToSByte(x));}
static int usignum(ushort x){return x < 0 ? ssignum16(Convert.ToInt16(-x)) : ssignum16(Convert.ToInt16(x));}
static int usignum(uint x){return x < 0 ? ssignum32(Convert.ToInt32(-x)) : ssignum32(Convert.ToInt32(x));}
static int usignum(ulong x){return x < 0 ? ssignum64(Convert.ToInt64(0-x)) : ssignum64(Convert.ToInt64(x));}

static float sitofp_i8_f32(sbyte x){return Convert.ToSingle(x);}
static float sitofp_i16_f32(short x){return Convert.ToSingle(x);}
static float sitofp_i32_f32(int x){return Convert.ToSingle(x);}
static float sitofp_i64_f32(long x){return Convert.ToSingle(x);}

static double sitofp_i8_f64(sbyte x){return Convert.ToDouble(x);}
static double sitofp_i16_f64(short x){return Convert.ToDouble(x);}
static double sitofp_i32_f64(int x){return Convert.ToDouble(x);}
static double sitofp_i64_f64(long x){return Convert.ToDouble(x);}


static float uitofp_i8_f32(sbyte x){return Convert.ToSingle(unsigned(x));}
static float uitofp_i16_f32(short x){return Convert.ToSingle(unsigned(x));}
static float uitofp_i32_f32(int x){return Convert.ToSingle(unsigned(x));}
static float uitofp_i64_f32(long x){return Convert.ToSingle(unsigned(x));}

static double uitofp_i8_f64(sbyte x){return Convert.ToDouble(unsigned(x));}
static double uitofp_i16_f64(short x){return Convert.ToDouble(unsigned(x));}
static double uitofp_i32_f64(int x){return Convert.ToDouble(unsigned(x));}
static double uitofp_i64_f64(long x){return Convert.ToDouble(unsigned(x));}

static byte fptoui_f32_i8(float x){return Convert.ToByte(Math.Truncate(x));}
static byte fptoui_f64_i8(double x){return Convert.ToByte(Math.Truncate(x));}
static sbyte fptosi_f32_i8(float x){return Convert.ToSByte(Math.Truncate(x));}
static sbyte fptosi_f64_i8(double x){return Convert.ToSByte(Math.Truncate(x));}

static ushort fptoui_f32_i16(float x){return Convert.ToUInt16(Math.Truncate(x));}
static ushort fptoui_f64_i16(double x){return Convert.ToUInt16(Math.Truncate(x));}
static short fptosi_f32_i16(float x){return Convert.ToInt16(Math.Truncate(x));}
static short fptosi_f64_i16(double x){return Convert.ToInt16(Math.Truncate(x));}

static uint fptoui_f32_i32(float x){return Convert.ToUInt32(Math.Truncate(x));}
static uint fptoui_f64_i32(double x){return Convert.ToUInt32(Math.Truncate(x));}
static int fptosi_f32_i32(float x){return Convert.ToInt32(Math.Truncate(x));}
static int fptosi_f64_i32(double x){return Convert.ToInt32(Math.Truncate(x));}

static ulong fptoui_f32_i64(float x){return Convert.ToUInt64(Math.Truncate(x));}
static ulong fptoui_f64_i64(double x){return Convert.ToUInt64(Math.Truncate(x));}
static long fptosi_f32_i64(float x){return Convert.ToInt64(Math.Truncate(x));}
static long fptosi_f64_i64(double x){return Convert.ToInt64(Math.Truncate(x));}

static double fpconv_f32_f64(float x){return Convert.ToDouble(x);}
static float fpconv_f64_f32(double x){return Convert.ToSingle(x);}

static double futhark_log64(double x){return Math.Log(x);}
static double futhark_sqrt64(double x){return Math.Sqrt(x);}
static double futhark_exp64(double x){return Math.Exp(x);}
static double futhark_cos64(double x){return Math.Cos(x);}
static double futhark_sin64(double x){return Math.Sin(x);}
static double futhark_tan64(double x){return Math.Tan(x);}
static double futhark_acos64(double x){return Math.Acos(x);}
static double futhark_asin64(double x){return Math.Asin(x);}
static double futhark_atan64(double x){return Math.Atan(x);}
static double futhark_atan2_64(double x, double y){return Math.Atan2(x, y);}
static bool futhark_isnan64(double x){return double.IsNaN(x);}
static bool futhark_isinf64(double x){return double.IsInfinity(x);}
static long futhark_to_bits64(double x){return BitConverter.ToInt64(BitConverter.GetBytes(x),0);}
static double futhark_from_bits64(long x){return BitConverter.ToDouble(BitConverter.GetBytes(x),0);}

static float futhark_log32(float x){return (float) Math.Log(x);}
static float futhark_sqrt32(float x){return (float) Math.Sqrt(x);}
static float futhark_exp32(float x){return (float) Math.Exp(x);}
static float futhark_cos32(float x){return (float) Math.Cos(x);}
static float futhark_sin32(float x){return (float) Math.Sin(x);}
static float futhark_tan32(float x){return (float) Math.Tan(x);}
static float futhark_acos32(float x){return (float) Math.Acos(x);}
static float futhark_asin32(float x){return (float) Math.Asin(x);}
static float futhark_atan32(float x){return (float) Math.Atan(x);}
static float futhark_atan2_32(float x, float y){return (float) Math.Atan2(x, y);}
static bool futhark_isnan32(float x){return float.IsNaN(x);}
static bool futhark_isinf32(float x){return float.IsInfinity(x);}
static int futhark_to_bits32(float x){return BitConverter.ToInt32(BitConverter.GetBytes(x), 0);}
static float futhark_from_bits32(int x){return BitConverter.ToSingle(BitConverter.GetBytes(x), 0);}

