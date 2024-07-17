using System;
using System.IO;
using System.Net.Security;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.Boleto;

namespace ACBrLib.Boleto
{
    public sealed partial class ACBrBoleto : ACBrLibHandle
    {
        #region Constructors

        public ACBrBoleto(string eArqConfig = "", string eChaveCrypt = "") : base(IsWindows ? "ACBrBoleto64.dll" : "libacbrboleto64.so",
                                                                                  IsWindows ? "ACBrBoleto32.dll" : "libacbrboleto32.so")
        {
            var inicializar = GetMethod<Boleto_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ref libHandle, ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);

            Config = new ACBrBoletoConfig(this);
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<Boleto_Nome>();
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

                var method = GetMethod<Boleto_Versao>();
                var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        public ACBrBoletoConfig Config { get; }

        #endregion Properties

        #region Methods

        #region Ini

        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<Boleto_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<Boleto_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<Boleto_ConfigLerValor>();

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

            var method = GetMethod<Boleto_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig = "")
        {
            var importarConfig = GetMethod<Boleto_ConfigImportar>();
            var ret = ExecuteMethod(() => importarConfig(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_ConfigExportar>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #endregion Ini

        public void ConfigurarDados(params BoletoInfo[] infos)
        {
            var iniFile = new ACBrIniFile();
            foreach (var info in infos)
                info.WriteToIni(iniFile);

            ConfigurarDados(iniFile.ToString());
        }

        public void ConfigurarDados(string eArquivoIni)
        {
            var method = GetMethod<Boleto_ConfigurarDados>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoIni)));

            CheckResult(ret);
        }

        public void IncluirTitulos(params Titulo[] titulos)
        {
            var iniFile = new ACBrIniFile();
            for (var i = 0; i < titulos.Length; i++)
            {
                titulos[i].Index = i + 1;
                titulos[i].WriteToIni(iniFile);
            }

            IncluirTitulos(iniFile.ToString());
        }

        public void IncluirTitulos(BoletoTpSaida eTpSaida, params Titulo[] titulos)
        {
            var iniFile = new ACBrIniFile();
            for (var i = 0; i < titulos.Length; i++)
            {
                titulos[i].Index = i + 1;
                titulos[i].WriteToIni(iniFile);
            }

            IncluirTitulos(iniFile.ToString(), eTpSaida);
        }

        public void IncluirTitulos(string eArquivoIni, BoletoTpSaida? eTpSaida = null)
        {
            var tpSaida = $"{(eTpSaida.HasValue ? (char)eTpSaida.Value : ' ')}";

            var method = GetMethod<Boleto_IncluirTitulos>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eArquivoIni), ToUTF8(tpSaida)));

            CheckResult(ret);
        }

        public void LimparLista()
        {
            var method = GetMethod<Boleto_LimparLista>();
            var ret = ExecuteMethod(() => method(libHandle));
            CheckResult(ret);
        }

        public int TotalTitulosLista()
        {
            var method = GetMethod<Boleto_TotalTitulosLista>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);

            return ret;
        }

        public void Imprimir(string eNomeImpressora = "")
        {
            var method = GetMethod<Boleto_Imprimir>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eNomeImpressora)));
            CheckResult(ret);
        }

        public void Imprimir(int indice, string eNomeImpressora = "")
        {
            var method = GetMethod<Boleto_ImprimirBoleto>();
            var ret = ExecuteMethod(() => method(libHandle, indice, ToUTF8(eNomeImpressora)));
            CheckResult(ret);
        }

        public void GerarPDF()
        {
            var method = GetMethod<Boleto_GerarPDF>();
            var ret = ExecuteMethod(() => method(libHandle));
            CheckResult(ret);
        }

        public void GerarPDF(Stream aStream)
        {
            if (aStream == null) throw new ArgumentNullException(nameof(aStream));

            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_SalvarPDF>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            var pdf = ProcessResult(buffer, bufferLen);
            Base64ToStream(pdf, aStream);
        }

        public void GerarPDF(int indice)
        {
            var method = GetMethod<Boleto_GerarPDFBoleto>();
            var ret = ExecuteMethod(() => method(libHandle, indice));
            CheckResult(ret);
        }

        public void GerarPDF(int indice, Stream aStream)
        {
            if (aStream == null) throw new ArgumentNullException(nameof(aStream));

            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_SalvarPDFBoleto>();
            var ret = ExecuteMethod(() => method(libHandle, indice, buffer, ref bufferLen));

            CheckResult(ret);

            var pdf = ProcessResult(buffer, bufferLen);
            Base64ToStream(pdf, aStream);
        }

        public void GerarHTML()
        {
            var method = GetMethod<Boleto_GerarHTML>();
            var ret = ExecuteMethod(() => method(libHandle));
            CheckResult(ret);
        }

        public void GerarRemessa(string eDir, int eNumArquivo, string eNomeArq)
        {
            var method = GetMethod<Boleto_GerarRemessa>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eDir), eNumArquivo, ToUTF8(eNomeArq)));

            CheckResult(ret);
        }

        public void GerarRemessaStream(int eNumArquivo, Stream aStream)
        {
            if (aStream == null) throw new ArgumentNullException(nameof(aStream));

            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_GerarRemessaStream>();
            var ret = ExecuteMethod(() => method(libHandle, eNumArquivo, buffer, ref bufferLen));

            CheckResult(ret);

            var rem = ProcessResult(buffer, bufferLen);
            Base64ToStream(rem, aStream);
        }

        public RetornoBoleto ObterRetorno(string eDir, string eNomeArq)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_ObterRetorno>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eDir), ToUTF8(eNomeArq), buffer, ref bufferLen));

            CheckResult(ret);

            return RetornoBoleto.LerRetorno(ProcessResult(buffer, bufferLen));
        }

        public void LerRetorno(string eDir, string eNomeArq)
        {
            var method = GetMethod<Boleto_LerRetorno>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eDir), ToUTF8(eNomeArq)));

            CheckResult(ret);
        }

        public string LerRetornoStream(string ARetornoBase64)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_LerRetornoStream>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(ARetornoBase64), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void EnviarEmail(string ePara, string eAssunto, string eMensagem, string eCC)
        {
            var method = GetMethod<Boleto_EnviarEmail>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(ePara), ToUTF8(eAssunto), ToUTF8(eMensagem), ToUTF8(eCC)));

            CheckResult(ret);
        }

        public void EnviarEmailBoleto(int eIndex, string ePara, string eAssunto, string eMensagem, string eCC)
        {
            var method = GetMethod<Boleto_EnviarEmailBoleto>();
            var ret = ExecuteMethod(() => method(libHandle, eIndex, ToUTF8(ePara), ToUTF8(eAssunto), ToUTF8(eMensagem), ToUTF8(eCC)));

            CheckResult(ret);
        }

        public void SetDiretorioArquivo(string eDir, string eArq = "")
        {
            var method = GetMethod<Boleto_SetDiretorioArquivo>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eDir), ToUTF8(eArq)));

            CheckResult(ret);
        }

        public string[] ListaBancos()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_ListaBancos>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen).Split('|');
        }

        public string[] ListaCaractTitulo()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_ListaCaractTitulo>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen).Split('|');
        }

        public string[] ListaOcorrencias()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_ListaOcorrencias>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen).Split('|');
        }

        public string[] ListaOcorrenciasEX()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_ListaOcorrenciasEX>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen).Split('|');
        }

        public int TamNossoNumero(string eCarteira, string enossoNumero, string eConvenio)
        {
            var method = GetMethod<Boleto_TamNossoNumero>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eCarteira), ToUTF8(enossoNumero), ToUTF8(eConvenio)));

            CheckResult(ret);

            return ret;
        }

        public string CodigosMoraAceitos()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_CodigosMoraAceitos>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void SelecionaBanco(string eCodBanco)
        {
            var method = GetMethod<Boleto_SelecionaBanco>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eCodBanco)));

            CheckResult(ret);
        }

        public string MontarNossoNumero(int eIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_MontarNossoNumero>();
            var ret = ExecuteMethod(() => method(libHandle, eIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string RetornaLinhaDigitavel(int eIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_RetornaLinhaDigitavel>();
            var ret = ExecuteMethod(() => method(libHandle, eIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string RetornaCodigoBarras(int eIndex)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_RetornaCodigoBarras>();
            var ret = ExecuteMethod(() => method(libHandle, eIndex, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public RetornoWeb EnviarBoleto(OperacaoBoleto opercao)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_EnviarBoleto>();
            var ret = ExecuteMethod(() => method(libHandle, (int)opercao, buffer, ref bufferLen));

            CheckResult(ret);

            return RetornoWeb.LerRetorno(ProcessResult(buffer, bufferLen));
        }

        public string ConsultarTitulosPorPeriodo(string eArquivoIni)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Boleto_ConsultarTitulosPorPeriodo>();
            var ret = ExecuteMethod<int>(() => method(libHandle, eArquivoIni, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<Boleto_Finalizar>();
            var ret = ExecuteMethod(() => finalizar(libHandle));
            CheckResult(ret);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<Boleto_UltimoRetorno>();

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