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
        public delegate int PIXCD_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_OpenSSLInfo(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_IncluirConta(IntPtr handle, string aInfIncluirConta, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ConsultarConta(IntPtr handle, string aAccountId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_InativarConta(IntPtr handle, string aAccountId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_IncluirChavePix(IntPtr handle, string aAccountId, string aExternalID, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ConsultarChavePix(IntPtr handle, string aAccountId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ExcluirChavePix(IntPtr handle, string aAccountId, string aChavePIX, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_GerarQRCode(IntPtr handle, string aInfQRCode, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ConsultarTransacao(IntPtr handle, string aAccountId, string aTransactionID, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ConsultarSaldoEC(IntPtr handle, string aAccountId, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ConsultarExtratoEC(IntPtr handle, string aAccountId, DateTime aInicio, DateTime aFim, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ConsultarMotivosDevolucao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_SolicitarDevolucao(IntPtr handle, string aInfSolicitarDevolucao, string aAccountId, string aTransactionID, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_ConsultarAliasRetirada(IntPtr handle, string aAccountId, string aAlias, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int PIXCD_Matera_SolicitarRetirada(IntPtr handle, string aInfSolicitarRetirada, string aAccountId, StringBuilder buffer, ref int bufferSize);

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