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
        public delegate int Sedex_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_Consultar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int Sedex_Rastrear(string eCodRastreio, StringBuilder buffer, ref int bufferSize);

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