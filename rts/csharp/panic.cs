void panic(int exitcode, string str, params Object[] args)
{
    var prog_name = Environment.GetCommandLineArgs()[0];
    Console.Write(String.Format("{0}:", prog_name));
    Console.Write(String.Format(str, args));
    Environment.Exit(exitcode);
}
