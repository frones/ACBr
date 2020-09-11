using System;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.GNRe
{
    public sealed class ACBrGNRe : ACBrLibHandle
    {
        #region InnerTypes

        private class Delegates
        {
            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_Finalizar(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_ConfigImportar(IntPtr handle, string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_ConfigLer(IntPtr handle, string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_ConfigGravar(IntPtr handle, string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_CarregarXML(IntPtr handle, string eArquivoOuXml);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_CarregarINI(IntPtr handle, string eArquivoOuIni);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_ObterXml(IntPtr handle, int AIndex, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_GravarXml(IntPtr handle, int AIndex, string eNomeArquivo, string ePathArquivo);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_CarregarGuiaRetorno(IntPtr handle, string eArquivoOuXml);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_LimparLista(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_LimparListaGuiaRetorno(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_Assinar(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_Validar(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_VerificarAssinatura(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_ObterCertificados(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_Enviar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_Consultar(IntPtr handle, string eUF, int AReceita, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_EnviarEmail(IntPtr handle, string ePara, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eCc, string eAnexos, string eMensagem);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_Imprimir(IntPtr handle, string eNomeImpressora, string eMostrarPreview);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int GNRE_ImprimirPDF(IntPtr handle);
        }

        #endregion InnerTypes

        #region Constructors

        public ACBrGNRe(string eArqConfig = "", string eChaveCrypt = "") : base("ACBrGNRe64.dll", "ACBrGNRe32.dll")
        {
            var inicializar = GetMethod<Delegates.GNRE_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ref libHandle, ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

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

                var method = GetMethod<Delegates.GNRE_Nome>();
                var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

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

                var method = GetMethod<Delegates.GNRE_Versao>();
                var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        #endregion Properties

        #region Methods

        #region Ini

        public void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<Delegates.GNRE_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<Delegates.GNRE_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<Delegates.GNRE_ConfigLerValor>();

            var bufferLen = BUFFER_LEN;
            var pValue = new StringBuilder(bufferLen);
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), pValue, ref bufferLen));
            CheckResult(ret);

            var value = ProcessResult(pValue, bufferLen);
            return ConvertValue<T>(value);
        }

        public void ConfigGravarValor(ACBrSessao eSessao, string eChave, object value)
        {
            if (value == null) return;

            var method = GetMethod<Delegates.GNRE_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        public void ImportarConfig(string eArqConfig = "")
        {
            var importarConfig = GetMethod<Delegates.GNRE_ConfigImportar>();
            var ret = ExecuteMethod(() => importarConfig(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.GNRE_ConfigExportar>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void CarregarXML(string eArquivoOuXml)
        {
            var method = GetMethod<Delegates.GNRE_CarregarXML>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void CarregarINI(string eArquivoOuIni)
        {
            var method = GetMethod<Delegates.GNRE_CarregarINI>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoOuIni)));

            CheckResult(ret);
        }

        public string ObterXml(int aIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.GNRE_ObterXml>();
            var ret = ExecuteMethod(() => method(libHandle, aIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void GravarXml(int aIndex, string eNomeArquivo = "", string ePathArquivo = "")
        {
            var method = GetMethod<Delegates.GNRE_GravarXml>();
            var ret = ExecuteMethod(() => method(libHandle, aIndex, ToUTF8(eNomeArquivo), ToUTF8(ePathArquivo)));

            CheckResult(ret);
        }

        public void CarregarGuiaRetorno(string eArquivoOuXml)
        {
            var method = GetMethod<Delegates.GNRE_CarregarGuiaRetorno>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoOuXml)));

            CheckResult(ret);
        }

        public void LimparLista()
        {
            var method = GetMethod<Delegates.GNRE_LimparLista>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void LimparListaGuiaRetorno()
        {
            var method = GetMethod<Delegates.GNRE_LimparListaGuiaRetorno>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void Assinar()
        {
            var method = GetMethod<Delegates.GNRE_Assinar>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void Validar()
        {
            var method = GetMethod<Delegates.GNRE_Validar>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public string VerificarAssinatura()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.GNRE_VerificarAssinatura>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public InfoCertificado[] ObterCertificados()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.GNRE_ObterCertificados>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            var certificados = ProcessResult(buffer, bufferLen).Split(new[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);
            return certificados.Length == 0 ? new InfoCertificado[0] : certificados.Select(x => new InfoCertificado(x)).ToArray();
        }

        public string Enviar()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.GNRE_Enviar>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string Consultar(string uf, int receita)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.GNRE_Consultar>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(uf), receita, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void EnviarEmail(string ePara, string eChaveNFe, bool aEnviaPDF, string eAssunto, string eMensagem, string[] eCc = null, string[] eAnexos = null)
        {
            var method = GetMethod<Delegates.GNRE_EnviarEmail>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(ePara), ToUTF8(eChaveNFe), aEnviaPDF, ToUTF8(eAssunto), ToUTF8(eCc == null ? "" : string.Join(";", eCc)),
                ToUTF8(eAnexos == null ? "" : string.Join(";", eAnexos)), ToUTF8(eMensagem.Replace(Environment.NewLine, ";"))));

            CheckResult(ret);
        }

        public void Imprimir(string impressora = "", bool? MostrarPreview = null)
        {
            var mostrarPreview = MostrarPreview.HasValue ? $"{Convert.ToInt32(MostrarPreview.Value)}" : string.Empty;

            var method = GetMethod<Delegates.GNRE_Imprimir>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(impressora), ToUTF8(mostrarPreview)));

            CheckResult(ret);
        }

        public void ImprimirPDF()
        {
            var method = GetMethod<Delegates.GNRE_ImprimirPDF>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<Delegates.GNRE_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar(libHandle));
            CheckResult(codRet);
        }

        protected override void InitializeMethods()
        {
            AddMethod<Delegates.GNRE_Inicializar>("GNRE_Inicializar");
            AddMethod<Delegates.GNRE_Finalizar>("GNRE_Finalizar");
            AddMethod<Delegates.GNRE_Nome>("GNRE_Nome");
            AddMethod<Delegates.GNRE_Versao>("GNRE_Versao");
            AddMethod<Delegates.GNRE_UltimoRetorno>("GNRE_UltimoRetorno");
            AddMethod<Delegates.GNRE_ConfigImportar>("GNRE_ConfigImportar");
            AddMethod<Delegates.GNRE_ConfigExportar>("GNRE_ConfigExportar");
            AddMethod<Delegates.GNRE_ConfigLer>("GNRE_ConfigLer");
            AddMethod<Delegates.GNRE_ConfigGravar>("GNRE_ConfigGravar");
            AddMethod<Delegates.GNRE_ConfigLerValor>("GNRE_ConfigLerValor");
            AddMethod<Delegates.GNRE_ConfigGravarValor>("GNRE_ConfigGravarValor");
            AddMethod<Delegates.GNRE_CarregarXML>("GNRE_CarregarXML");
            AddMethod<Delegates.GNRE_CarregarINI>("GNRE_CarregarINI");
            AddMethod<Delegates.GNRE_ObterXml>("GNRE_ObterXml");
            AddMethod<Delegates.GNRE_GravarXml>("GNRE_GravarXml");
            AddMethod<Delegates.GNRE_CarregarGuiaRetorno>("GNRE_CarregarGuiaRetorno");
            AddMethod<Delegates.GNRE_LimparLista>("GNRE_LimparLista");
            AddMethod<Delegates.GNRE_LimparListaGuiaRetorno>("GNRE_LimparListaGuiaRetorno");
            AddMethod<Delegates.GNRE_Assinar>("GNRE_Assinar");
            AddMethod<Delegates.GNRE_Validar>("GNRE_Validar");
            AddMethod<Delegates.GNRE_VerificarAssinatura>("GNRE_VerificarAssinatura");
            AddMethod<Delegates.GNRE_ObterCertificados>("GNRE_ObterCertificados");
            AddMethod<Delegates.GNRE_Enviar>("GNRE_Enviar");
            AddMethod<Delegates.GNRE_Consultar>("GNRE_Consultar");
            AddMethod<Delegates.GNRE_EnviarEmail>("GNRE_EnviarEmail");
            AddMethod<Delegates.GNRE_Imprimir>("GNRE_Imprimir");
            AddMethod<Delegates.GNRE_ImprimirPDF>("GNRE_ImprimirPDF");
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<Delegates.GNRE_UltimoRetorno>();

            if (iniBufferLen < 1)
            {
                ExecuteMethod(() => ultimoRetorno(libHandle, buffer, ref bufferLen));
                if (bufferLen <= BUFFER_LEN) return FromUTF8(buffer);

                buffer.Capacity = bufferLen;
            }

            ExecuteMethod(() => ultimoRetorno(libHandle, buffer, ref bufferLen));
            return FromUTF8(buffer);
        }

        #endregion Private Methods

        #endregion Methods
    }
}