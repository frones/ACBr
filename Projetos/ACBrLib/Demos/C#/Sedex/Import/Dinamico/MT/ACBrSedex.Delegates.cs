using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Sedex
{
    public sealed partial class ACBrSedex
    {
		
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_Consultar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_Rastrear(IntPtr handle, string eCodRastreio, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<Sedex_Inicializar>("Sedex_Inicializar");
            AddMethod<Sedex_Finalizar>("Sedex_Finalizar");
            AddMethod<Sedex_Nome>("Sedex_Nome");
            AddMethod<Sedex_Versao>("Sedex_Versao");
            AddMethod<Sedex_UltimoRetorno>("Sedex_UltimoRetorno");
            AddMethod<Sedex_ConfigImportar>("Sedex_ConfigImportar");
            AddMethod<Sedex_ConfigExportar>("Sedex_ConfigExportar");
            AddMethod<Sedex_ConfigLer>("Sedex_ConfigLer");
            AddMethod<Sedex_ConfigGravar>("Sedex_ConfigGravar");
            AddMethod<Sedex_ConfigLerValor>("Sedex_ConfigLerValor");
            AddMethod<Sedex_ConfigGravarValor>("Sedex_ConfigGravarValor");
            AddMethod<Sedex_Consultar>("Sedex_Consultar");
            AddMethod<Sedex_Rastrear>("Sedex_Rastrear");
        }
    }
}