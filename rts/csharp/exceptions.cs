public class TypeError : Exception
{
    public TypeError(){}
    public TypeError(string message):base(message){}
    public TypeError(string message, Exception inner):base(message, inner){}
}