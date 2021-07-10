using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.CEP
{
    public sealed partial class ACBrCEP
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CEP_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CEP_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CEP_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CEP_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CEP_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CEP_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CEP_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CEP_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CEP_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CEP_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CEP_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CEP_BuscarPorCEP(string eCEP, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CEP_BuscarPorLogradouro(string eCidade, string eTipo_Logradouro, string eLogradouro, string eUF, string eBairro, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<CEP_Inicializar>("CEP_Inicializar");
            AddMethod<CEP_Finalizar>("CEP_Finalizar");
            AddMethod<CEP_Nome>("CEP_Nome");
            AddMethod<CEP_Versao>("CEP_Versao");
            AddMethod<CEP_UltimoRetorno>("CEP_UltimoRetorno");
            AddMethod<CEP_ConfigImportar>("CEP_ConfigImportar");
            AddMethod<CEP_ConfigExportar>("CEP_ConfigExportar");
            AddMethod<CEP_ConfigLer>("CEP_ConfigLer");
            AddMethod<CEP_ConfigGravar>("CEP_ConfigGravar");
            AddMethod<CEP_ConfigLerValor>("CEP_ConfigLerValor");
            AddMethod<CEP_ConfigGravarValor>("CEP_ConfigGravarValor");
            AddMethod<CEP_BuscarPorCEP>("CEP_BuscarPorCEP");
            AddMethod<CEP_BuscarPorLogradouro>("CEP_BuscarPorLogradouro");
        }
    }
}