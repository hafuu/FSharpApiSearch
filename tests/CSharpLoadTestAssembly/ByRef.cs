using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpLoadTestAssembly
{
    public class ByRef
    {
        public static ref int F(ref int a, out string b)
        {
            b = "";
            return ref a;
        }
    }
}
