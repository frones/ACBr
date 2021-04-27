using System;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.Boleto;

namespace ACBrLib.Boleto
{
    public sealed class ACBrBoleto : ACBrLibHandle
    {
        #region InnerTypes

        private class Delegates
        {
            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_Finalizar(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_ConfigImportar(IntPtr handle, string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_ConfigLer(IntPtr handle, string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_ConfigGravar(IntPtr handle, string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_ConfigurarDados(IntPtr handle, string eArquivoIni, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_IncluirTitulos(IntPtr handle, string eArquivoIni, string eTpSaida, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_LimparLista(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_TotalTitulosLista(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_Imprimir(IntPtr handle, string eNomeImpressora);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_ImprimirBoleto(IntPtr handle, int eIndice, string eNomeImpressora);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_GerarPDF(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_GerarHTML(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_GerarRemessa(IntPtr handle, string eDir, int eNumArquivo, string eNomeArq);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_LerRetorno(IntPtr handle, string eDir, string eNomeArq);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_EnviarEmail(IntPtr handle, string ePara, string eAssunto, string eMensagem, string eCC);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_EnviarEmailBoleto(IntPtr handle, int eIndice, string ePara, string eAssunto, string eMensagem, string eCC);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_SetDiretorioArquivo(IntPtr handle, string eDir, string eArq, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_ListaBancos(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_ListaCaractTitulo(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_ListaOcorrencias(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_ListaOcorrenciasEX(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_TamNossoNumero(IntPtr handle, string eCarteira, string enossoNumero, string eConvenio, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_CodigosMoraAceitos(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_SelecionaBanco(IntPtr handle, string eCodBanco, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_MontarNossoNumero(IntPtr handle, int eIndice, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_RetornaLinhaDigitavel(IntPtr handle, int eIndice, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int Boleto_RetornaCodigoBarras(IntPtr handle, int eIndice, StringBuilder buffer, ref int bufferSize);
        }

        #endregion InnerTypes

        #region Constructors

        public ACBrBoleto(string eArqConfig = "", string eChaveCrypt = "") :
            base("ACBrBoleto64.dll", "ACBrBoleto32.dll")
        {
            var inicializar = GetMethod<Delegates.Boleto_Inicializar>();
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

                var method = GetMethod<Delegates.Boleto_Nome>();
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

                var method = GetMethod<Delegates.Boleto_Versao>();
                var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        #endregion Properties

        #region Methods

        #region Ini

        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<Delegates.Boleto_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<Delegates.Boleto_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<Delegates.Boleto_ConfigLerValor>();

            var bufferLen = BUFFER_LEN;
            var pValue = new StringBuilder(bufferLen);
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), pValue, ref bufferLen));
            CheckResult(ret);

            var value = ProcessResult(pValue, bufferLen);
            return ConvertValue<T>(value);
        }

        public override void ConfigGravarValor(ACBrSessao eSessao, string eChave, object value)
        {
            if (value == null) return;

            var method = GetMethod<Delegates.Boleto_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig = "")
        {
            var importarConfig = GetMethod<Delegates.Boleto_ConfigImportar>();
            var ret = ExecuteMethod(() => importarConfig(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.Boleto_ConfigExportar>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #endregion Ini

        public string ConfigurarDados(string eArquivoIni)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.Boleto_ConfigurarDados>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoIni), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string IncluirTitulos(string eArquivoIni, BoletoTpSaida? eTpSaida = null)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);
            var tpSaida = $"{(eTpSaida.HasValue ? (char)eTpSaida.Value : ' ')}";

            var method = GetMethod<Delegates.Boleto_IncluirTitulos>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoIni), ToUTF8(tpSaida), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void LimparLista()
        {
            var method = GetMethod<Delegates.Boleto_LimparLista>();
            var ret = ExecuteMethod(() => method(libHandle));
            CheckResult(ret);
        }

        public string TotalTitulosLista()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.Boleto_TotalTitulosLista>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void Imprimir(string eNomeImpressora = "")
        {
            var method = GetMethod<Delegates.Boleto_Imprimir>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eNomeImpressora)));
            CheckResult(ret);
        }

        public void ImprimirBoleto(int indice, string eNomeImpressora = "")
        {
            var method = GetMethod<Delegates.Boleto_ImprimirBoleto>();
            var ret = ExecuteMethod(() => method(libHandle, indice, ToUTF8(eNomeImpressora)));
            CheckResult(ret);
        }

        public void GerarPDF()
        {
            var method = GetMethod<Delegates.Boleto_GerarPDF>();
            var ret = ExecuteMethod(() => method(libHandle));
            CheckResult(ret);
        }

        public void GerarHTML()
        {
            var method = GetMethod<Delegates.Boleto_GerarHTML>();
            var ret = ExecuteMethod(() => method(libHandle));
            CheckResult(ret);
        }

        public void GerarRemessa(string eDir, int eNumArquivo, string eNomeArq)
        {
            var method = GetMethod<Delegates.Boleto_GerarRemessa>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eDir), eNumArquivo, ToUTF8(eNomeArq)));

            CheckResult(ret);
        }

        public void LerRetorno(string eDir, string eNomeArq)
        {
            var method = GetMethod<Delegates.Boleto_LerRetorno>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eDir), ToUTF8(eNomeArq)));

            CheckResult(ret);
        }

        public void EnviarEmail(string ePara, string eAssunto, string eMensagem, string eCC)
        {
            var method = GetMethod<Delegates.Boleto_EnviarEmail>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(ePara), ToUTF8(eAssunto), ToUTF8(eMensagem), ToUTF8(eCC)));

            CheckResult(ret);
        }

        public void EnviarEmailBoleto(int eIndex, string ePara, string eAssunto, string eMensagem, string eCC)
        {
            var method = GetMethod<Delegates.Boleto_EnviarEmailBoleto>();
            var ret = ExecuteMethod(() => method(libHandle, eIndex, ToUTF8(ePara), ToUTF8(eAssunto), ToUTF8(eMensagem), ToUTF8(eCC)));

            CheckResult(ret);
        }

        public string SetDiretorioArquivo(string eDir, string eArq)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.Boleto_SetDiretorioArquivo>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eDir), ToUTF8(eArq), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ListaBancos()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.Boleto_ListaBancos>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ListaCaractTitulo()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.Boleto_ListaCaractTitulo>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ListaOcorrencias()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.Boleto_ListaOcorrencias>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ListaOcorrenciasEX()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.Boleto_ListaOcorrenciasEX>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string TamNossoNumero(string eCarteira, string enossoNumero, string eConvenio)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.Boleto_TamNossoNumero>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eCarteira), ToUTF8(enossoNumero), ToUTF8(eConvenio), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string CodigosMoraAceitos()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.Boleto_CodigosMoraAceitos>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string SelecionaBanco(string eCodBanco)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.Boleto_SelecionaBanco>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eCodBanco), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string MontarNossoNumero(int eIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.Boleto_MontarNossoNumero>();
            var ret = ExecuteMethod(() => method(libHandle, eIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string RetornaLinhaDigitavel(int eIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.Boleto_RetornaLinhaDigitavel>();
            var ret = ExecuteMethod(() => method(libHandle, eIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string RetornaCodigoBarras(int eIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Delegates.Boleto_RetornaCodigoBarras>();
            var ret = ExecuteMethod(() => method(libHandle, eIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #region Private Methods

        protected override void InitializeMethods()
        {
            AddMethod<Delegates.Boleto_Inicializar>("Boleto_Inicializar");
            AddMethod<Delegates.Boleto_Finalizar>("Boleto_Finalizar");
            AddMethod<Delegates.Boleto_Nome>("Boleto_Nome");
            AddMethod<Delegates.Boleto_Versao>("Boleto_Versao");
            AddMethod<Delegates.Boleto_UltimoRetorno>("Boleto_UltimoRetorno");
            AddMethod<Delegates.Boleto_ConfigImportar>("Boleto_ConfigImportar");
            AddMethod<Delegates.Boleto_ConfigExportar>("Boleto_ConfigExportar");
            AddMethod<Delegates.Boleto_ConfigLer>("Boleto_ConfigLer");
            AddMethod<Delegates.Boleto_ConfigGravar>("Boleto_ConfigGravar");
            AddMethod<Delegates.Boleto_ConfigLerValor>("Boleto_ConfigLerValor");
            AddMethod<Delegates.Boleto_ConfigGravarValor>("Boleto_ConfigGravarValor");
            AddMethod<Delegates.Boleto_ConfigurarDados>("Boleto_ConfigurarDados");
            AddMethod<Delegates.Boleto_IncluirTitulos>("Boleto_IncluirTitulos");
            AddMethod<Delegates.Boleto_LimparLista>("Boleto_LimparLista");
            AddMethod<Delegates.Boleto_TotalTitulosLista>("Boleto_TotalTitulosLista");
            AddMethod<Delegates.Boleto_Imprimir>("Boleto_Imprimir");
            AddMethod<Delegates.Boleto_ImprimirBoleto>("Boleto_ImprimirBoleto");
            AddMethod<Delegates.Boleto_GerarPDF>("Boleto_GerarPDF");
            AddMethod<Delegates.Boleto_GerarHTML>("Boleto_GerarHTML");
            AddMethod<Delegates.Boleto_GerarRemessa>("Boleto_GerarRemessa");
            AddMethod<Delegates.Boleto_LerRetorno>("Boleto_LerRetorno");
            AddMethod<Delegates.Boleto_EnviarEmail>("Boleto_EnviarEmail");
            AddMethod<Delegates.Boleto_EnviarEmailBoleto>("Boleto_EnviarEmailBoleto");
            AddMethod<Delegates.Boleto_SetDiretorioArquivo>("Boleto_SetDiretorioArquivo");
            AddMethod<Delegates.Boleto_ListaBancos>("Boleto_ListaBancos");
            AddMethod<Delegates.Boleto_ListaCaractTitulo>("Boleto_ListaCaractTitulo");
            AddMethod<Delegates.Boleto_ListaOcorrencias>("Boleto_ListaOcorrencias");
            AddMethod<Delegates.Boleto_ListaOcorrenciasEX>("Boleto_ListaOcorrenciasEX");
            AddMethod<Delegates.Boleto_TamNossoNumero>("Boleto_TamNossoNumero");
            AddMethod<Delegates.Boleto_CodigosMoraAceitos>("Boleto_CodigosMoraAceitos");
            AddMethod<Delegates.Boleto_SelecionaBanco>("Boleto_SelecionaBanco");
            AddMethod<Delegates.Boleto_MontarNossoNumero>("Boleto_MontarNossoNumero");
            AddMethod<Delegates.Boleto_RetornaLinhaDigitavel>("Boleto_RetornaLinhaDigitavel");
            AddMethod<Delegates.Boleto_RetornaCodigoBarras>("Boleto_RetornaCodigoBarras");
        }

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<Delegates.Boleto_Finalizar>();
            var ret = ExecuteMethod(() => finalizar(libHandle));
            CheckResult(ret);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<Delegates.Boleto_UltimoRetorno>();

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