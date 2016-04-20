using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpLoadTestAssembly
{
    public interface Interface
    {
        int Method(int x, string y);
        int Property { get; set; }
    }

    public interface GenericInterface<T>
    {
        int Method(T x);
        T Property { set; }
    }
}
