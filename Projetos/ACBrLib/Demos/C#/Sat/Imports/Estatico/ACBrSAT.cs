using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;

namespace ACBrLib.Sat
{
    public sealed class ACBrSat : ACBrLibHandle
    {
        #region InnerTypes

        private class Delegates
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
        }

        #endregion InnerTypes

        #region Constructors

        public ACBrSat(string eArqConfig = "", string eChaveCrypt = "") :
            base(Environment.Is64BitProcess ? "ACBrSAT64.dll" : "ACBrSAT32.dll")
        {
            var inicializar = GetMethod<Delegates.SAT_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<Delegates.SAT_Nome>();
                var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        public string Versao
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<Delegates.SAT_Versao>();
                var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        #endregion Properties

        #region Methods

        #region Ini

        public void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<Delegates.SAT_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<Delegates.SAT_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<Delegates.SAT_ConfigLerValor>();

            var bufferLen = BUFFER_LEN;
            var pValue = new StringBuilder(bufferLen);
            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), pValue, ref bufferLen));
            CheckResult(ret);

            var value = ProcessResult(pValue, bufferLen);
            return ConvertValue<T>(value);
        }

        public void ConfigGravarValor(ACBrSessao eSessao, string eChave, object value)
        {
            if (value == null) return;

            var method = GetMethod<Delegates.SAT_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        public void Inicializar()
        {
            var method = GetMethod<SAT_InicializarSAT>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void DesInicializar()
        {
            var method = GetMethod<SAT_DesInicializar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public AtivarSatResposta AtivarSAT(string CNPJValue, int cUF)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_AtivarSAT>();
            var ret = ExecuteMethod(() => method(ToUTF8(CNPJValue), cUF, buffer, ref bufferLen));

            CheckResult(ret);

            return AtivarSatResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public AssociarAssinaturaResposta AssociarAssinatura(string CNPJValue, string assinaturaCNPJs)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_AssociarAssinatura>();
            var ret = ExecuteMethod(() => method(ToUTF8(CNPJValue), ToUTF8(assinaturaCNPJs), buffer, ref bufferLen));

            CheckResult(ret);

            return AssociarAssinaturaResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public BloquearSatResposta BloquearSAT()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_BloquearSAT>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return BloquearSatResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public DesbloquearSatResposta DesbloquearSAT()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_DesbloquearSAT>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return DesbloquearSatResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public TrocarCodigoDeAtivacaoResposta TrocarCodigoDeAtivacao(string codigoDeAtivacaoOuEmergencia, int opcao, string novoCodigo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_TrocarCodigoDeAtivacao>();
            var ret = ExecuteMethod(() => method(ToUTF8(codigoDeAtivacaoOuEmergencia), opcao,
                ToUTF8(novoCodigo), buffer, ref bufferLen));

            CheckResult(ret);

            return TrocarCodigoDeAtivacaoResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public ConsultarSatResposta ConsultarSAT()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_ConsultarSAT>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ConsultarSatResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public ConsultarStatusOperacionalResposta ConsultarStatusOperacional()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_ConsultarStatusOperacional>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ConsultarStatusOperacionalResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public ConsultarSessaoSatResposta ConsultarNumeroSessao(int cNumeroDeSessao)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_ConsultarNumeroSessao>();
            var ret = ExecuteMethod(() => method(cNumeroDeSessao, buffer, ref bufferLen));

            CheckResult(ret);

            return ConsultarSessaoSatResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public ConsultarUltimaSessaoFiscalResposta ConsultarUltimaSessaoFiscal()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_ConsultarUltimaSessaoFiscal>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ConsultarUltimaSessaoFiscalResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public AtualizarSoftwareSatResposta AtualizarSoftwareSAT()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_AtualizarSoftwareSAT>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return AtualizarSoftwareSatResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public ComunicarCertificadoICPBRASILResposta ComunicarCertificadoICPBRASIL(string certificado)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_ComunicarCertificadoICPBRASIL>();
            var ret = ExecuteMethod(() => method(ToUTF8(certificado), buffer, ref bufferLen));

            CheckResult(ret);

            return ComunicarCertificadoICPBRASILResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public void ExtrairLogs(string eArquivo)
        {
            var method = GetMethod<SAT_ExtrairLogs>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivo)));

            CheckResult(ret);
        }

        public TesteFimAFimResposta TesteFimAFim(string eArquivoXmlVenda)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_TesteFimAFim>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXmlVenda), buffer, ref bufferLen));

            CheckResult(ret);

            return TesteFimAFimResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public string GerarAssinaturaSAT(string eCNPJSHW, string eCNPJEmitente)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_GerarAssinaturaSAT>();
            var ret = ExecuteMethod(() => method(ToUTF8(eCNPJSHW), ToUTF8(eCNPJEmitente), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public CFeResposta CriarCFe(CupomFiscal CFe) => CriarCFe(CFe.ToString());

        public CFeResposta CriarCFe(string eArquivoIni)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_CriarCFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoIni), buffer, ref bufferLen));

            CheckResult(ret);

            return CFeResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public void validarCFe(string eArquivoXml)
        {
            var method = GetMethod<SAT_ValidarCFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml)));

            CheckResult(ret);
        }

        public EnvioResposta CriarEnviarCFe(CupomFiscal CFe) => CriarEnviarCFe(CFe.ToString());

        public EnvioResposta CriarEnviarCFe(string eArquivoIni)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_CriarEnviarCFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoIni), buffer, ref bufferLen));

            CheckResult(ret);

            return EnvioResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public EnvioResposta EnviarCFe(string eArquivoXml)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_EnviarCFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), buffer, ref bufferLen));

            CheckResult(ret);

            return EnvioResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public CancelarCFeResposta CancelarCFe(string eArquivoXml)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_CancelarCFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), buffer, ref bufferLen));

            CheckResult(ret);

            return CancelarCFeResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public void ImprimirExtratoVenda(string eArquivoXml, string eNomeImpressora = "")
        {
            var method = GetMethod<SAT_ImprimirExtratoVenda>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), ToUTF8(eNomeImpressora)));

            CheckResult(ret);
        }

        public void ImprimirExtratoResumido(string eArquivoXml, string eNomeImpressora = "")
        {
            var method = GetMethod<SAT_ImprimirExtratoResumido>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), ToUTF8(eNomeImpressora)));

            CheckResult(ret);
        }

        public PDFExtratoVendaResposta GerarPDFExtratoVenda(string eArquivoXml, string eNomeArquivo = "")
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_GerarPDFExtratoVenda>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), ToUTF8(eNomeArquivo), buffer, ref bufferLen));

            CheckResult(ret);

            return PDFExtratoVendaResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public PDFExtratoVendaResposta GerarPDFCancelamento(string eArqXMLVenda, string eArqXMLCancelamento, string eNomeArquivo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_GerarPDFCancelamento>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArqXMLVenda), ToUTF8(eArqXMLCancelamento), ToUTF8(eNomeArquivo), buffer, ref bufferLen));

            CheckResult(ret);

            return PDFExtratoVendaResposta.LerResposta(ProcessResult(buffer, bufferLen));
        }

        public string GerarImpressaoFiscalMFe(string eArquivoXml)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_GerarImpressaoFiscalMFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void ImprimirExtratoCancelamento(string eArqXMLVenda, string eArqXMLCancelamento, string eNomeImpressora = "")
        {
            var method = GetMethod<SAT_ImprimirExtratoCancelamento>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArqXMLVenda), ToUTF8(eArqXMLCancelamento), ToUTF8(eNomeImpressora)));

            CheckResult(ret);
        }

        public void EnviarEmail(string eArquivoXml, string ePara, string eAssunto, string eNomeArquivo,
            string sMensagem, string sCC, string eAnexos)
        {
            var method = GetMethod<SAT_EnviarEmail>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), ToUTF8(ePara), ToUTF8(eAssunto),
                ToUTF8(eNomeArquivo), ToUTF8(sMensagem), ToUTF8(sCC), ToUTF8(eAnexos)));

            CheckResult(ret);
        }

        public async void SalvarPDF(Stream aStream)
        {
            if (aStream == null) throw new ArgumentNullException(nameof(aStream));

            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<SAT_SalvarPDF>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            var pdf = ProcessResult(buffer, bufferLen);
            Base64ToStream(pdf, aStream);
        }

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<Delegates.SAT_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar());
            CheckResult(codRet);
        }

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
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<Delegates.SAT_UltimoRetorno>();

            if (iniBufferLen < 1)
            {
                ExecuteMethod(() => ultimoRetorno(buffer, ref bufferLen));
                if (bufferLen <= BUFFER_LEN) return FromUTF8(buffer);

                buffer.Capacity = bufferLen;
            }

            ExecuteMethod(() => ultimoRetorno(buffer, ref bufferLen));
            return FromUTF8(buffer);
        }

        #endregion Private Methods

        #endregion Methods
    }
}