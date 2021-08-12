using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.IBGE
{
    public sealed partial class ACBrIBGE
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_BuscarPorCodigo(IntPtr handle, int ACodMun, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int IBGE_BuscarPorNome(IntPtr handle, string eCidade, string eUF, bool Exata, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<IBGE_Inicializar>("IBGE_Inicializar");
            AddMethod<IBGE_Finalizar>("IBGE_Finalizar");
            AddMethod<IBGE_Nome>("IBGE_Nome");
            AddMethod<IBGE_Versao>("IBGE_Versao");
            AddMethod<IBGE_UltimoRetorno>("IBGE_UltimoRetorno");
            AddMethod<IBGE_ConfigImportar>("IBGE_ConfigImportar");
            AddMethod<IBGE_ConfigExportar>("IBGE_ConfigExportar");
            AddMethod<IBGE_ConfigLer>("IBGE_ConfigLer");
            AddMethod<IBGE_ConfigGravar>("IBGE_ConfigGravar");
            AddMethod<IBGE_ConfigLerValor>("IBGE_ConfigLerValor");
            AddMethod<IBGE_ConfigGravarValor>("IBGE_ConfigGravarValor");
            AddMethod<IBGE_BuscarPorCodigo>("IBGE_BuscarPorCodigo");
            AddMethod<IBGE_BuscarPorNome>("IBGE_BuscarPorNome");
        }
    }
}