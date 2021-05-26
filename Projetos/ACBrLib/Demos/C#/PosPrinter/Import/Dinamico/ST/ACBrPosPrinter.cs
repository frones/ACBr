using System;
using System.Globalization;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.PosPrinter;

namespace ACBrLib.PosPrinter
{
    /// <inheritdoc />
    public sealed partial class ACBrPosPrinter : ACBrLibHandle
    {
        #region Constructors

        public ACBrPosPrinter(string eArqConfig = "", string eChaveCrypt = "") : base(IsWindows ? "ACBrPosPrinter64.dll" : "libacbrposprinter64.so",
                                                                                      IsWindows ? "ACBrPosPrinter32.dll" : "libacbrposprinter32.so")
        {
            var inicializar = GetMethod<POS_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);

            Config = new PosPrinterConfig(this);
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<POS_Nome>();
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

                var method = GetMethod<POS_Versao>();
                var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        public PosPrinterConfig Config { get; }

        #endregion Properties

        #region Metodos

        #region Ini

        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<POS_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig)
        {
            var lerIni = GetMethod<POS_ConfigImportar>();
            var ret = ExecuteMethod(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<POS_ConfigExportar>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<POS_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<POS_ConfigLerValor>();

            var bufferLen = BUFFER_LEN;
            var pValue = new StringBuilder(bufferLen);
            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), pValue, ref bufferLen));
            CheckResult(ret);

            var value = ProcessResult(pValue, bufferLen);
            return ConvertValue<T>(value);
        }

        public override void ConfigGravarValor(ACBrSessao eSessao, string eChave, object value)
        {
            if (value == null) return;

            var method = GetMethod<POS_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        #region Ativar

        public void Ativar()
        {
            var method = GetMethod<POS_Ativar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Desativar()
        {
            var method = GetMethod<POS_Desativar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        #endregion Ativar

        #region Diversos

        public void Zerar()
        {
            var method = GetMethod<POS_Zerar>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Inicializar()
        {
            var method = GetMethod<POS_InicializarPos>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Reset()
        {
            var method = GetMethod<POS_Reset>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void PularLinhas(int numLinhas = 0)
        {
            var method = GetMethod<POS_PularLinhas>();
            var ret = ExecuteMethod(() => method(numLinhas));

            CheckResult(ret);
        }

        public void CortarPapel(bool parcial = false)
        {
            var method = GetMethod<POS_CortarPapel>();
            var ret = ExecuteMethod(() => method(parcial));

            CheckResult(ret);
        }

        public void AbrirGaveta()
        {
            var method = GetMethod<POS_AbrirGaveta>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public string LerInfoImpressora()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<POS_LerInfoImpressora>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public ACBrPosTipoStatus LerStatusImpressora(int tentativas = 1)
        {
            var status = 0;
            var method = GetMethod<POS_LerStatusImpressora>();
            var ret = ExecuteMethod(() => method(tentativas, ref status));

            CheckResult(ret);

            return (ACBrPosTipoStatus)status;
        }

        public string[] RetornarTags(bool incluiAjuda = true)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<POS_RetornarTags>();
            var ret = ExecuteMethod(() => method(incluiAjuda, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen).Split('|');
        }

        public string[] AcharPortas()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<POS_AcharPortas>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);
            var portas = ProcessResult(buffer, bufferLen);

            return portas.Split('|')
                         .Where(x => !string.IsNullOrEmpty(x) && !string.IsNullOrWhiteSpace(x))
                         .ToArray();
        }

        public void GravarLogoArquivo(string aPath, int nAKC1, int nAKC2)
        {
            var method = GetMethod<POS_GravarLogoArquivo>();
            var ret = ExecuteMethod(() => method(ToUTF8(aPath), nAKC1, nAKC2));

            CheckResult(ret);
        }

        public void ApagarLogo(int nAKC1, int nAKC2)
        {
            var method = GetMethod<POS_ApagarLogo>();
            var ret = ExecuteMethod(() => method(nAKC1, nAKC2));

            CheckResult(ret);
        }

        public string LeituraCheque()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<POS_LeituraCheque>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string LerCMC7(bool AguardaCheque, int SegundosEspera)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<POS_LerCMC7>();
            var ret = ExecuteMethod(() => method(AguardaCheque, SegundosEspera, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void EjetarCheque()
        {
            var method = GetMethod<POS_EjetarCheque>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public bool PodeLerDaPorta()
        {
            var method = GetMethod<POS_PodeLerDaPorta>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);

            return ret == 1;
        }

        public string LerCaracteristicas()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<POS_LerCaracteristicas>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #endregion Diversos

        #region Imprimir

        public void Imprimir(string aString = "", bool pulaLinha = false, bool decodificarTags = true, bool codificarPagina = true, int copias = 1)
        {
            var method = GetMethod<POS_Imprimir>();
            var ret = ExecuteMethod(() => method(ToUTF8(aString), pulaLinha, decodificarTags, codificarPagina, copias));

            CheckResult(ret);
        }

        public void ImprimirLinha(string aString)
        {
            var method = GetMethod<POS_ImprimirLinha>();
            var ret = ExecuteMethod(() => method(ToUTF8(aString)));

            CheckResult(ret);
        }

        public void ImprimirCmd(string aString)
        {
            var method = GetMethod<POS_ImprimirCmd>();
            var ret = ExecuteMethod(() => method(ToUTF8(aString)));

            CheckResult(ret);
        }

        public void ImprimirTags()
        {
            var method = GetMethod<POS_ImprimirTags>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void ImprimirImagemArquivo(string aPath)
        {
            var method = GetMethod<POS_ImprimirImagemArquivo>();
            var ret = ExecuteMethod(() => method(ToUTF8(aPath)));

            CheckResult(ret);
        }

        public void ImprimirLogo(int nAKC1, int nAKC2, int nFatorX, int nFatorY)
        {
            var method = GetMethod<POS_ImprimirLogo>();
            var ret = ExecuteMethod(() => method(nAKC1, nAKC2, nFatorX, nFatorY));

            CheckResult(ret);
        }

        public void ImprimirCheque(int CodBanco, decimal AValor, DateTime ADataEmissao, string AFavorecido,
            string ACidade, string AComplemento, bool LerCMC7, int SegundosEspera)
        {
            var method = GetMethod<POS_ImprimirCheque>();

            var valor = AValor.ToString("N2", CultureInfo.CreateSpecificCulture("pt-BR"));
            var data = ADataEmissao.ToString("dd/MM/yyyy");

            var ret = ExecuteMethod(() => method(CodBanco, ToUTF8(valor), ToUTF8(data), ToUTF8(AFavorecido), ToUTF8(ACidade),
                                                 ToUTF8(AComplemento), LerCMC7, SegundosEspera));

            CheckResult(ret);
        }

        public void ImprimirTextoCheque(int X, int Y, string AString, bool AguardaCheque, int SegundosEspera)
        {
            var method = GetMethod<POS_ImprimirTextoCheque>();
            var ret = ExecuteMethod(() => method(X, Y, ToUTF8(AString), AguardaCheque, SegundosEspera));

            CheckResult(ret);
        }

        public string TxRx(string aString, byte bytesToRead = 1, int aTimeOut = 500, bool waitForTerminator = false)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<POS_TxRx>();
            var ret = ExecuteMethod(() => method(ToUTF8(aString), bytesToRead, aTimeOut, waitForTerminator, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #endregion Imprimir

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<POS_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar());
            CheckResult(codRet);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<POS_UltimoRetorno>();

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

        #endregion Metodos
    }
}