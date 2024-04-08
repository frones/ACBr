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
        public delegate int GTIN_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_OpenSSLInfo(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int GTIN_Consultar(string aGTIN, StringBuilder buffer, ref int bufferSize);

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