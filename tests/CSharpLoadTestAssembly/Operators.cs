using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpLoadTestAssembly
{
    public class Operators
    {
        public static Operators operator+ (Operators x, Operators y) => x;
        public static implicit operator Operators (string x) => new Operators();
    }
}
