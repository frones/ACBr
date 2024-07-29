using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.PIXCD
{
    public sealed partial class ACBrPIXCD
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_OpenSSLInfo(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_IncluirConta(string aInfIncluirConta, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ConsultarConta(string aAccountId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_InativarConta(string aAccountId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_IncluirChavePix(string aAccountId, string aExternalID, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ConsultarChavePix(string aAccountId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ExcluirChavePix(string aAccountId, string aChavePIX, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_GerarQRCode(string aInfQRCode, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ConsultarTransacao(string aAccountId, string aTransactionID, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ConsultarSaldoEC(string aAccountId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ConsultarExtratoEC(string aAccountId, DateTime aInicio, DateTime aFim, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ConsultarMotivosDevolucao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_SolicitarDevolucao(string aInfSolicitarDevolucao, string aAccountId, string aTransactionID, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ConsultarAliasRetirada(string aAccountId, string aAlias, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_SolicitarRetirada(string aInfSolicitarRetirada, string aAccountId, StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<PIXCD_Inicializar>("PIXCD_Inicializar");
            AddMethod<PIXCD_Finalizar>("PIXCD_Finalizar");
            AddMethod<PIXCD_Nome>("PIXCD_Nome");
            AddMethod<PIXCD_Versao>("PIXCD_Versao");
            AddMethod<PIXCD_UltimoRetorno>("PIXCD_UltimoRetorno");
            AddMethod<PIXCD_ConfigImportar>("PIXCD_ConfigImportar");
            AddMethod<PIXCD_ConfigExportar>("PIXCD_ConfigExportar");
            AddMethod<PIXCD_ConfigLer>("PIXCD_ConfigLer");
            AddMethod<PIXCD_ConfigGravar>("PIXCD_ConfigGravar");
            AddMethod<PIXCD_ConfigLerValor>("PIXCD_ConfigLerValor");
            AddMethod<PIXCD_ConfigGravarValor>("PIXCD_ConfigGravarValor");
            AddMethod<PIXCD_OpenSSLInfo>("PIXCD_OpenSSLInfo");
            AddMethod<PIXCD_Matera_IncluirConta>("PIXCD_Matera_IncluirConta");
            AddMethod<PIXCD_Matera_ConsultarConta>("PIXCD_Matera_ConsultarConta");
            AddMethod<PIXCD_Matera_InativarConta>("PIXCD_Matera_InativarConta");
            AddMethod<PIXCD_Matera_IncluirChavePix>("PIXCD_Matera_IncluirChavePix");
            AddMethod<PIXCD_Matera_ConsultarChavePix>("PIXCD_Matera_ConsultarChavePix");
            AddMethod<PIXCD_Matera_ExcluirChavePix>("PIXCD_Matera_ExcluirChavePix");
            AddMethod<PIXCD_Matera_GerarQRCode>("PIXCD_Matera_GerarQRCode");
            AddMethod<PIXCD_Matera_ConsultarTransacao>("PIXCD_Matera_ConsultarTransacao");
            AddMethod<PIXCD_Matera_ConsultarSaldoEC>("PIXCD_Matera_ConsultarSaldoEC");
            AddMethod<PIXCD_Matera_ConsultarExtratoEC>("PIXCD_Matera_ConsultarExtratoEC");
            AddMethod<PIXCD_Matera_ConsultarMotivosDevolucao>("PIXCD_Matera_ConsultarMotivosDevolucao");
            AddMethod<PIXCD_Matera_SolicitarDevolucao>("PIXCD_Matera_SolicitarDevolucao");
            AddMethod<PIXCD_Matera_ConsultarAliasRetirada>("PIXCD_Matera_ConsultarAliasRetirada");
            AddMethod<PIXCD_Matera_SolicitarRetirada>("PIXCD_Matera_SolicitarRetirada");
        }
    }
}