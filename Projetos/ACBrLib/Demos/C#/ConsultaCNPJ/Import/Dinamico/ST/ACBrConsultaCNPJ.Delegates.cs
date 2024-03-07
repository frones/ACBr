using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.ConsultaCNPJ
{
    public sealed partial class ACBrConsultaCNPJ
    {
        public object MessageBox { get; private set; }

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CNPJ_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CNPJ_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CNPJ_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CNPJ_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CNPJ_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CNPJ_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CNPJ_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CNPJ_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CNPJ_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CNPJ_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CNPJ_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CNPJ_ConsultarCaptcha(string ePathDownload, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int CNPJ_Consultar(string eCNPJ, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<CNPJ_Inicializar>("CNPJ_Inicializar");
            AddMethod<CNPJ_Finalizar>("CNPJ_Finalizar");
            AddMethod<CNPJ_Nome>("CNPJ_Nome");
            AddMethod<CNPJ_Versao>("CNPJ_Versao");
            AddMethod<CNPJ_UltimoRetorno>("CNPJ_UltimoRetorno");
            AddMethod<CNPJ_ConfigImportar>("CNPJ_ConfigImportar");
            AddMethod<CNPJ_ConfigExportar>("CNPJ_ConfigExportar");
            AddMethod<CNPJ_ConfigLer>("CNPJ_ConfigLer");
            AddMethod<CNPJ_ConfigGravar>("CNPJ_ConfigGravar");
            AddMethod<CNPJ_ConfigLerValor>("CNPJ_ConfigLerValor");
            AddMethod<CNPJ_ConfigGravarValor>("CNPJ_ConfigGravarValor");
            AddMethod<CNPJ_ConsultarCaptcha>("CNPJ_ConsultarCaptcha");
            AddMethod<CNPJ_Consultar>("CNPJ_Consultar");
        }


    }
}