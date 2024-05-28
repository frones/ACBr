using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Sat
{
    public sealed partial class ACBrSat
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_InicializarSAT(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_DesInicializar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_AtivarSAT(IntPtr handle, string CNPJValue, int cUF, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_AssociarAssinatura(IntPtr handle, string CNPJValue, string assinaturaCNPJs, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_BloquearSAT(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_DesbloquearSAT(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_TrocarCodigoDeAtivacao(IntPtr handle, string codigoDeAtivacaoOuEmergencia, int opcao, string novoCodigo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConsultarSAT(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConsultarStatusOperacional(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConsultarNumeroSessao(IntPtr handle, int cNumeroDeSessao, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConsultarUltimaSessaoFiscal(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_AtualizarSoftwareSAT(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ComunicarCertificadoICPBRASIL(IntPtr handle, string certificado, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ExtrairLogs(IntPtr handle, string eArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_TesteFimAFim(IntPtr handle, string eArquivoXmlVenda, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_GerarAssinaturaSAT(IntPtr handle, string eCNPJSHW, string eCNPJEmitente, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_CriarCFe(IntPtr handle, string eArquivoIni, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_CriarEnviarCFe(IntPtr handle, string eArquivoIni, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_EnviarCFe(IntPtr handle, string eArquivoXml, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ValidarCFe(IntPtr handle, string eArquivoXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_CancelarCFe(IntPtr handle, string eArquivoXml, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ImprimirExtratoVenda(IntPtr handle, string eArquivoXml, string eNomeImpressora);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ImprimirExtratoResumido(IntPtr handle, string eArquivoXml, string eNomeImpressora);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_GerarPDFExtratoVenda(IntPtr handle, string eArquivoXml, string eNomeArquivo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_GerarPDFCancelamento(IntPtr handle, string eArqXMLVenda, string eArqXMLCancelamento, string eNomeArquivo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_GerarImpressaoFiscalMFe(IntPtr handle, string eArquivoXml, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ImprimirExtratoCancelamento(IntPtr handle, string eArqXMLVenda, string eArqXMLCancelamento, string eNomeImpressora);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_EnviarEmail(IntPtr handle, string eArquivoXml, string ePara, string eAssunto, string eNomeArquivo,
            string sMensagem, string sCC, string eAnexos);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_SalvarPDF(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_SetNumeroSessao(IntPtr handle, string cNumeroSessao);

        protected override void InitializeMethods()
        {
            AddMethod<SAT_Inicializar>("SAT_Inicializar");
            AddMethod<SAT_Finalizar>("SAT_Finalizar");
            AddMethod<SAT_Nome>("SAT_Nome");
            AddMethod<SAT_Versao>("SAT_Versao");
            AddMethod<SAT_UltimoRetorno>("SAT_UltimoRetorno");
            AddMethod<SAT_ConfigImportar>("SAT_ConfigImportar");
            AddMethod<SAT_ConfigExportar>("SAT_ConfigExportar");
            AddMethod<SAT_ConfigLer>("SAT_ConfigLer");
            AddMethod<SAT_ConfigGravar>("SAT_ConfigGravar");
            AddMethod<SAT_ConfigLerValor>("SAT_ConfigLerValor");
            AddMethod<SAT_ConfigGravarValor>("SAT_ConfigGravarValor");
            AddMethod<SAT_InicializarSAT>("SAT_InicializarSAT");
            AddMethod<SAT_DesInicializar>("SAT_DesInicializar");
            AddMethod<SAT_AtivarSAT>("SAT_AtivarSAT");
            AddMethod<SAT_AssociarAssinatura>("SAT_AssociarAssinatura");
            AddMethod<SAT_BloquearSAT>("SAT_BloquearSAT");
            AddMethod<SAT_DesbloquearSAT>("SAT_DesbloquearSAT");
            AddMethod<SAT_TrocarCodigoDeAtivacao>("SAT_TrocarCodigoDeAtivacao");
            AddMethod<SAT_ConsultarSAT>("SAT_ConsultarSAT");
            AddMethod<SAT_ConsultarStatusOperacional>("SAT_ConsultarStatusOperacional");
            AddMethod<SAT_ConsultarNumeroSessao>("SAT_ConsultarNumeroSessao");
            AddMethod<SAT_ConsultarUltimaSessaoFiscal>("SAT_ConsultarUltimaSessaoFiscal");
            AddMethod<SAT_AtualizarSoftwareSAT>("SAT_AtualizarSoftwareSAT");
            AddMethod<SAT_ComunicarCertificadoICPBRASIL>("SAT_ComunicarCertificadoICPBRASIL");
            AddMethod<SAT_ExtrairLogs>("SAT_ExtrairLogs");
            AddMethod<SAT_TesteFimAFim>("SAT_TesteFimAFim");
            AddMethod<SAT_GerarAssinaturaSAT>("SAT_GerarAssinaturaSAT");
            AddMethod<SAT_CriarCFe>("SAT_CriarCFe");
            AddMethod<SAT_ValidarCFe>("SAT_ValidarCFe");
            AddMethod<SAT_CriarEnviarCFe>("SAT_CriarEnviarCFe");
            AddMethod<SAT_EnviarCFe>("SAT_EnviarCFe");
            AddMethod<SAT_CancelarCFe>("SAT_CancelarCFe");
            AddMethod<SAT_ImprimirExtratoVenda>("SAT_ImprimirExtratoVenda");
            AddMethod<SAT_ImprimirExtratoResumido>("SAT_ImprimirExtratoResumido");
            AddMethod<SAT_GerarPDFExtratoVenda>("SAT_GerarPDFExtratoVenda");
            AddMethod<SAT_GerarPDFCancelamento>("SAT_GerarPDFCancelamento");
            AddMethod<SAT_GerarImpressaoFiscalMFe>("SAT_GerarImpressaoFiscalMFe");
            AddMethod<SAT_ImprimirExtratoCancelamento>("SAT_ImprimirExtratoCancelamento");
            AddMethod<SAT_EnviarEmail>("SAT_EnviarEmail");
            AddMethod<SAT_SalvarPDF>("SAT_SalvarPDF");
            AddMethod<SAT_SetNumeroSessao>("SAT_SetNumeroSessao");
        }
    }
}