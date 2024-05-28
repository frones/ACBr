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
        public delegate int SAT_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_InicializarSAT();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_DesInicializar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_AtivarSAT(string CNPJValue, int cUF, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_AssociarAssinatura(string CNPJValue, string assinaturaCNPJs, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_BloquearSAT(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_DesbloquearSAT(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_TrocarCodigoDeAtivacao(string codigoDeAtivacaoOuEmergencia, int opcao, string novoCodigo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConsultarSAT(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConsultarStatusOperacional(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConsultarNumeroSessao(int cNumeroDeSessao, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ConsultarUltimaSessaoFiscal(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_AtualizarSoftwareSAT(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ComunicarCertificadoICPBRASIL(string certificado, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ExtrairLogs(string eArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_TesteFimAFim(string eArquivoXmlVenda, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_GerarAssinaturaSAT(string eCNPJSHW, string eCNPJEmitente, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_CriarCFe(string eArquivoIni, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ValidarCFe(string eArquivoXml);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_CriarEnviarCFe(string eArquivoIni, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_EnviarCFe(string eArquivoXml, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_CancelarCFe(string eArquivoXml, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ImprimirExtratoVenda(string eArquivoXml, string eNomeImpressora);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ImprimirExtratoResumido(string eArquivoXml, string eNomeImpressora);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_GerarPDFExtratoVenda(string eArquivoXml, string eNomeArquivo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_GerarPDFCancelamento(string eArqXMLVenda, string eArqXMLCancelamento, string eNomeArquivo, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_GerarImpressaoFiscalMFe(string eArquivoXml, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_ImprimirExtratoCancelamento(string eArqXMLVenda, string eArqXMLCancelamento, string eNomeImpressora);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_EnviarEmail(string eArquivoXml, string ePara, string eAssunto, string eNomeArquivo,
            string sMensagem, string sCC, string eAnexos);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_SalvarPDF(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int SAT_SetNumeroSessao(string cNumeroSessao);

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
            AddMethod< SAT_SetNumeroSessao>("SAT_SetNumeroSessao");
        }
    }
}