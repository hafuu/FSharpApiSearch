using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpLoadTestAssembly
{
    public class StaticMemberClass
    {
        public StaticMemberClass() { }

        public StaticMemberClass(string x, string y) { }

        public static int[] ArrayMethod() => null;
        public static int[,] Array2dMethod() => null;
        public static int[,][] NestedArrayMethod() => new[,] { { new int[0], new int[0] } };

        public static int NoParameterMethod() => 3;
        public static void NonCurriedMethod(int x, string y) { }
        public static void TupleMethod(Tuple<int, string> x) { }

        public static int OverloadMethod(int x) => x;
        public static string OverloadMethod(string x) => x;

        public static string Getter { get; }
        public static string Setter { set { } }
        public static string GetterSetter { get; set; }

        public static int Field;
    }

    public class InstanceMemberClass
    {
        public int NoParameterMethod() => 3;
        public void NonCurriedMethod(int x, string y) { }
        public void TupleMethod(Tuple<int, string> x) { }

        public int OverloadMethod(int x) => x;
        public string OverloadMethod(string x) => x;

        public string Getter { get; }
        public string Setter { set { } }
        public string GetterSetter { get; set; }

        public int Field;

        protected string ProtectedMethod() => "";
    }

    public class IndexedGetter
    {
        public int this[int x] { get { return x; } }
    }

    public class IndexedSetter
    {
        public int this[int x] { set { } }
    }

    public class IndexedGetterSetter
    {
        public int this[int x] { get { return x; } set { } }
    }

    public struct Struct
    {
        public int Field;
    }
}
