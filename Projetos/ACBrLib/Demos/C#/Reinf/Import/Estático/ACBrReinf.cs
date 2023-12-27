using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.Reinf
{
    /// <inheritdoc />
    public sealed partial class ACBrReinf : ACBrLibHandle
    {
        #region Constructors

        public ACBrReinf(string eArqConfig = "", string eChaveCrypt = "") : base(IsWindows ? "ACBrReinf64.dll" : "libacbrReinf64.so",
                                                                                      IsWindows ? "ACBrReinf32.dll" : "libacbrReinf32.so")
        {
            var inicializar = GetMethod<Reinf_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);

            Config = new ACBrReinfConfig(this);
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<Reinf_Nome>();
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

                var method = GetMethod<Reinf_Versao>();
                var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        public ACBrReinfConfig Config { get; }

        #endregion Properties

        #region Metodos

        #region Ini

        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<Reinf_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig)
        {
            var lerIni = GetMethod<Reinf_ConfigImportar>();
            var ret = ExecuteMethod(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Reinf_ConfigExportar>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<Reinf_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<Reinf_ConfigLerValor>();

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

            var method = GetMethod<Reinf_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        #region Diversos
        public void CriarEventoReinf (string eArqIni)
        {
            var method = GetMethod<Reinf_CriarEventoReinf>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArqIni)));

            CheckResult(ret);
        }

        public string EnviarReinf()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Reinf_EnviarReinf>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string ConsultarReinf(string eProtocolo)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Reinf_ConsultarReinf>();
            var ret = ExecuteMethod(() => method(ToUTF8(eProtocolo), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }
        public string ConsultarReciboReinf(string ePerApur, int aTipoEvento, string eNrInscEstab,
            string eCnpjPrestador, string eNrInscTomador, string eDtApur, string eCpfCnpjBenef,
            string eCnpjFonte)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Reinf_ConsultarReciboReinf>();
            var ret = ExecuteMethod(() => method(ToUTF8(ePerApur), aTipoEvento, ToUTF8(eNrInscEstab),
                                                 ToUTF8(eCnpjPrestador), ToUTF8(eNrInscTomador), 
                                                 ToUTF8(eDtApur), ToUTF8(eCpfCnpjBenef),
                                                 ToUTF8(eCnpjFonte),
                                                 buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string CriarEnviarReinf(string eArqIni)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Reinf_CriarEnviarReinf>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArqIni), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void LimparReinf()
        {
            var method = GetMethod<Reinf_LimparReinf>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void CarregarXMLEventoReinf(string eArquivoOuXML)
        {
            var method = GetMethod<Reinf_CarregarXMLEventoReinf>();
            var ret = ExecuteMethod(() => method(ToUTF8(eArquivoOuXML)));

            CheckResult(ret);
        }

        public void SetIDContribuinte(string aIdContribuinte)
        {
            var method = GetMethod<Reinf_SetIDContribuinte>();
            var ret = ExecuteMethod(() => method(ToUTF8(aIdContribuinte)));

            CheckResult(ret);
        }

        public void SetIDTransmissor(string aIdTransmissor)
        {
            var method = GetMethod<Reinf_SetIDTransmissor>();
            var ret = ExecuteMethod(() => method(ToUTF8(aIdTransmissor)));

            CheckResult(ret);
        }

        public void TipoContribuinte(int aTipoContribuinte)
        {
            var method = GetMethod<Reinf_SetTipoContribuinte>();
            var ret = ExecuteMethod(() => method(aTipoContribuinte));

            CheckResult(ret);
        }

        public void SetVersao(string sVersao)
        {
            var method = GetMethod<Reinf_SetVersaoDF>();
            var ret = ExecuteMethod(() => method(ToUTF8(sVersao)));

            CheckResult(ret);
        }

        public InfoCertificado[] ObterCertificados()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<Reinf_ObterCertificados>();
            var ret = ExecuteMethod(() => method(buffer, ref bufferLen));

            CheckResult(ret);

            var certificados = ProcessResult(buffer, bufferLen).Split(new[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);
            return certificados.Length == 0 ? new InfoCertificado[0] : certificados.Select(x => new InfoCertificado(x)).ToArray();
        }

        #endregion Diversos

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<Reinf_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar());
            CheckResult(codRet);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<Reinf_UltimoRetorno>();

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