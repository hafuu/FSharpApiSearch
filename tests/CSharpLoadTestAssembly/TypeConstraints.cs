using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpLoadTestAssembly
{
    public class TypeConstraints
    {
        public static void Struct<T>(T x) where T : struct { }
        public static void Class<T>(T x) where T : class { }
        public static void New<T>(T x) where T : new() { }
        public static void Subtype<T>(T x) where T : IComparable { }
        public static void VariableSubtype<T, U>(T x, U y) where T : U { }
    }
}
