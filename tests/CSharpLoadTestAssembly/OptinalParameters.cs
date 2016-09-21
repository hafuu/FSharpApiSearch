using Microsoft.FSharp.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpLoadTestAssembly
{
    public class OptinalParameters
    {
        public static void F(int x = 0) { }
        public static void G(FSharpOption<int> x = null) { }
    }
}
