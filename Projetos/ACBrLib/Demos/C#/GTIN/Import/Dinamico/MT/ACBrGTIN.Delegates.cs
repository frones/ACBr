using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.GTIN
{
    public sealed partial class ACBrGTIN
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_OpenSSLInfo(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_Consultar(IntPtr handle, string aGTIN, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<GTIN_Inicializar>("GTIN_Inicializar");
            AddMethod<GTIN_Finalizar>("GTIN_Finalizar");
            AddMethod<GTIN_Nome>("GTIN_Nome");
            AddMethod<GTIN_Versao>("GTIN_Versao");
            AddMethod<GTIN_OpenSSLInfo>("GTIN_OpenSSLInfo");
            AddMethod<GTIN_UltimoRetorno>("GTIN_UltimoRetorno");
            AddMethod<GTIN_ConfigImportar>("GTIN_ConfigImportar");
            AddMethod<GTIN_ConfigExportar>("GTIN_ConfigExportar");
            AddMethod<GTIN_ConfigLer>("GTIN_ConfigLer");
            AddMethod<GTIN_ConfigGravar>("GTIN_ConfigGravar");
            AddMethod<GTIN_ConfigLerValor>("GTIN_ConfigLerValor");
            AddMethod<GTIN_ConfigGravarValor>("GTIN_ConfigGravarValor");
            AddMethod<GTIN_Consultar>("GTIN_Consultar");

        }
    }
}