using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpLoadTestAssembly
{
    public class Tuples
    {
        public static Tuple<int, string> F(Tuple<int, string> x) => Tuple.Create(1, "a");
        public static (int, string) G((int, string) x) => (1, "a");
    }
}
