using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;

namespace ACBrLib.Sat
{
    public sealed class ACBrSat : ACBrLibHandle
    {
        #region Fields

        private const int BUFFER_LEN = 256;

        #endregion Fields

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
            public delegate int SAT_AtualizarSoftwareSAT(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int SAT_ComunicarCertificadoICPBRASIL(string certificado, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int SAT_ExtrairLogs(string eArquivo);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int SAT_TesteFimAFim(string eArquivoXmlVenda);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int SAT_GerarAssinaturaSAT(string eCNPJSHW, string eCNPJEmitente, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int SAT_CriarCFe(string eArquivoIni, StringBuilder buffer, ref int bufferSize);

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
            public delegate int SAT_GerarImpressaoFiscalMFe(string eArquivoXml, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int SAT_ImprimirExtratoCancelamento(string eArqXMLVenda, string eArqXMLCancelamento, string eNomeImpressora);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int SAT_EnviarEmail(string eArquivoXml, string ePara, string eAssunto, string eNomeArquivo,
                string sMensagem, string sCC, string eAnexos);
        }

        #endregion InnerTypes

        #region Constructors

        public ACBrSat(string eArqConfig = "", string eChaveCrypt = "") :
            base(Environment.Is64BitProcess ? "ACBrSAT64.dll" : "ACBrSAT32.dll")
        {
            InitializeMethods();

            var inicializar = GetMethod<Delegates.SAT_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);
        }

        #endregion Constructors

        #region Methods

        #region Ini

        public void ConfigGravar(string eArqConfig = "ACBrLib.ini")
        {
            var gravarIni = GetMethod<Delegates.SAT_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public void ConfigLer(string eArqConfig = "ACBrLib.ini")
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

            var value = FromUTF8(pValue);

            if (typeof(T).IsEnum) return (T)Enum.ToObject(typeof(T), Convert.ToInt32(value));

            if (typeof(T) == typeof(bool)) return (T)(object)Convert.ToBoolean(Convert.ToInt32(value));

            return (T)Convert.ChangeType(value, typeof(T));
        }

        public void ConfigGravarValor(ACBrSessao eSessao, string eChave, object value)
        {
            if (value == null) return;

            var method = GetMethod<Delegates.SAT_ConfigGravarValor>();
            var type = value.GetType();

            var propValue = value.ToString();
            if (type.IsEnum) propValue = ((int)value).ToString();
            if (type == typeof(bool)) propValue = Convert.ToInt32(value).ToString();

            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        public void Inicializar()
        {
            var method = GetMethod<Delegates.SAT_InicializarSAT>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void DesInicializar()
        {
            var method = GetMethod<Delegates.SAT_DesInicializar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public string AssociarAssinatura(string CNPJValue, string assinaturaCNPJs)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_AssociarAssinatura>();
            var ret = ExecuteMethod(() => method(ToUTF8(CNPJValue), ToUTF8(assinaturaCNPJs), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string BloquearSAT()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_BloquearSAT>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string DesbloquearSAT()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_DesbloquearSAT>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string TrocarCodigoDeAtivacao(string codigoDeAtivacaoOuEmergencia, int opcao, string novoCodigo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_TrocarCodigoDeAtivacao>();
            var ret = ExecuteMethod(() => method(ToUTF8(codigoDeAtivacaoOuEmergencia), opcao,
                ToUTF8(novoCodigo), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarSAT()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_ConsultarSAT>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarStatusOperacional()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_ConsultarStatusOperacional>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarNumeroSessao(int cNumeroDeSessao)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_ConsultarNumeroSessao>();
            var ret = ExecuteMethod(() => method(cNumeroDeSessao, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string AtualizarSoftwareSAT()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_AtualizarSoftwareSAT>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ComunicarCertificadoICPBRASIL(string certificado)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_ComunicarCertificadoICPBRASIL>();
            var ret = ExecuteMethod(() => method(ToUTF8(certificado), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void ExtrairLogs(string eArquivo)
        {
            var method = GetMethod<Delegates.SAT_ExtrairLogs>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivo)));

            CheckResult(ret);
        }

        public void TesteFimAFim(string eArquivoXmlVenda)
        {
            var method = GetMethod<Delegates.SAT_TesteFimAFim>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXmlVenda)));

            CheckResult(ret);
        }

        public string GerarAssinaturaSAT(string eCNPJSHW, string eCNPJEmitente)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_GerarAssinaturaSAT>();
            var ret = ExecuteMethod(() => method(ToUTF8(eCNPJSHW), ToUTF8(eCNPJEmitente), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string CriarCFe(string eArquivoIni)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_CriarCFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoIni), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string CriarEnviarCFe(string eArquivoIni)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_CriarEnviarCFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoIni), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string EnviarCFe(string eArquivoXml)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_EnviarCFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string CancelarCFe(string eArquivoXml)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_CancelarCFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void ImprimirExtratoVenda(string eArquivoXml, string eNomeImpressora = "")
        {
            var method = GetMethod<Delegates.SAT_ImprimirExtratoVenda>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), ToUTF8(eNomeImpressora)));

            CheckResult(ret);
        }

        public void ImprimirExtratoResumido(string eArquivoXml, string eNomeImpressora = "")
        {
            var method = GetMethod<Delegates.SAT_ImprimirExtratoResumido>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), ToUTF8(eNomeImpressora)));

            CheckResult(ret);
        }

        public string GerarPDFExtratoVenda(string eArquivoXml, string eNomeArquivo = "")
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_GerarPDFExtratoVenda>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), ToUTF8(eNomeArquivo), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string GerarImpressaoFiscalMFe(string eArquivoXml)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.SAT_GerarImpressaoFiscalMFe>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void ImprimirExtratoCancelamento(string eArqXMLVenda, string eArqXMLCancelamento, string eNomeImpressora = "")
        {
            var method = GetMethod<Delegates.SAT_ImprimirExtratoCancelamento>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArqXMLVenda), ToUTF8(eArqXMLCancelamento), ToUTF8(eNomeImpressora)));

            CheckResult(ret);
        }

        public void EnviarEmail(string eArquivoXml, string ePara, string eAssunto, string eNomeArquivo,
            string sMensagem, string sCC, string eAnexos)
        {
            var method = GetMethod<Delegates.SAT_EnviarEmail>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoXml), ToUTF8(ePara), ToUTF8(eAssunto),
                ToUTF8(eNomeArquivo), ToUTF8(sMensagem), ToUTF8(sCC), ToUTF8(eAnexos)));

            CheckResult(ret);
        }

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<Delegates.SAT_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar());
            CheckResult(codRet);
        }

        private void InitializeMethods()
        {
            AddMethod<Delegates.SAT_Inicializar>("SAT_Inicializar");
            AddMethod<Delegates.SAT_Finalizar>("SAT_Finalizar");
            AddMethod<Delegates.SAT_Nome>("SAT_Nome");
            AddMethod<Delegates.SAT_Versao>("SAT_Versao");
            AddMethod<Delegates.SAT_UltimoRetorno>("SAT_UltimoRetorno");
            AddMethod<Delegates.SAT_ConfigLer>("SAT_ConfigLer");
            AddMethod<Delegates.SAT_ConfigGravar>("SAT_ConfigGravar");
            AddMethod<Delegates.SAT_ConfigLerValor>("SAT_ConfigLerValor");
            AddMethod<Delegates.SAT_ConfigGravarValor>("SAT_ConfigGravarValor");
            AddMethod<Delegates.SAT_InicializarSAT>("SAT_InicializarSAT");
            AddMethod<Delegates.SAT_DesInicializar>("SAT_DesInicializar");
            AddMethod<Delegates.SAT_AssociarAssinatura>("SAT_AssociarAssinatura");
            AddMethod<Delegates.SAT_BloquearSAT>("SAT_BloquearSAT");
            AddMethod<Delegates.SAT_DesbloquearSAT>("SAT_DesbloquearSAT");
            AddMethod<Delegates.SAT_TrocarCodigoDeAtivacao>("SAT_TrocarCodigoDeAtivacao");
            AddMethod<Delegates.SAT_ConsultarSAT>("SAT_ConsultarSAT");
            AddMethod<Delegates.SAT_ConsultarStatusOperacional>("SAT_ConsultarStatusOperacional");
            AddMethod<Delegates.SAT_ConsultarNumeroSessao>("SAT_ConsultarNumeroSessao");
            AddMethod<Delegates.SAT_AtualizarSoftwareSAT>("SAT_AtualizarSoftwareSAT");
            AddMethod<Delegates.SAT_ComunicarCertificadoICPBRASIL>("SAT_ComunicarCertificadoICPBRASIL");
            AddMethod<Delegates.SAT_ExtrairLogs>("SAT_ExtrairLogs");
            AddMethod<Delegates.SAT_TesteFimAFim>("SAT_TesteFimAFim");
            AddMethod<Delegates.SAT_GerarAssinaturaSAT>("SAT_GerarAssinaturaSAT");
            AddMethod<Delegates.SAT_CriarCFe>("SAT_CriarCFe");
            AddMethod<Delegates.SAT_CriarEnviarCFe>("SAT_CriarEnviarCFe");
            AddMethod<Delegates.SAT_EnviarCFe>("SAT_EnviarCFe");
            AddMethod<Delegates.SAT_CancelarCFe>("SAT_CancelarCFe");
            AddMethod<Delegates.SAT_ImprimirExtratoVenda>("SAT_ImprimirExtratoVenda");
            AddMethod<Delegates.SAT_ImprimirExtratoResumido>("SAT_ImprimirExtratoResumido");
            AddMethod<Delegates.SAT_GerarPDFExtratoVenda>("SAT_GerarPDFExtratoVenda");
            AddMethod<Delegates.SAT_GerarImpressaoFiscalMFe>("SAT_GerarImpressaoFiscalMFe");
            AddMethod<Delegates.SAT_ImprimirExtratoCancelamento>("SAT_ImprimirExtratoCancelamento");
            AddMethod<Delegates.SAT_EnviarEmail>("SAT_EnviarEmail");
        }

        private static string ToUTF8(string value)
        {
            return string.IsNullOrEmpty(value) ? value : Encoding.Default.GetString(Encoding.UTF8.GetBytes(value));
        }

        private static string FromUTF8(StringBuilder value)
        {
            if (value == null) return null;
            return value.Length == 0
                ? string.Empty
                : Encoding.UTF8.GetString(Encoding.Default.GetBytes(value.ToString()));
        }

        private void CheckResult(int ret)
        {
            if (ret >= 0) return;

            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<Delegates.SAT_UltimoRetorno>();

            ExecuteMethod(() => ultimoRetorno(buffer, ref bufferLen));

            if (bufferLen > BUFFER_LEN)
            {
                buffer.Capacity = bufferLen;
                ExecuteMethod(() => ultimoRetorno(buffer, ref bufferLen));
            }

            switch (ret)
            {
                case -10:
                    throw new ApplicationException(FromUTF8(buffer));

                case -6:
                    throw new DirectoryNotFoundException(FromUTF8(buffer));

                case -5:
                    throw new FileNotFoundException(FromUTF8(buffer));

                case -4:
                    throw new ApplicationException(FromUTF8(buffer));

                case -3:
                    throw new ApplicationException(FromUTF8(buffer));

                case -2:
                    throw new ApplicationException(FromUTF8(buffer));

                case -1:
                    throw new ApplicationException(FromUTF8(buffer));
            }
        }

        private string ProcessResult(StringBuilder buffer, int bufferLen)
        {
            if (bufferLen > BUFFER_LEN)
            {
                buffer.Capacity = bufferLen;
                var ultimoRetorno = GetMethod<Delegates.SAT_UltimoRetorno>();
                ExecuteMethod(() => ultimoRetorno(buffer, ref bufferLen));
            }

            return FromUTF8(buffer);
        }

        #endregion Private Methods

        #endregion Methods
    }
}