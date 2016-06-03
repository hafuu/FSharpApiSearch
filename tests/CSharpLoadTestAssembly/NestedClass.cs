using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpLoadTestAssembly
{
    public class OuterClass
    {
        public OuterClass(){ }

        public class InnerClass
        {
            public InnerClass() { }
            public static int StaticMethod() => 3;
        }
    }

    public class GenericOuterClass<T>
    {
        public GenericOuterClass() { }

        public class GenericInnerClass<U>
        {
            public GenericInnerClass() { }

            public static void Method(T x, U y) { }
        }
    }
}
