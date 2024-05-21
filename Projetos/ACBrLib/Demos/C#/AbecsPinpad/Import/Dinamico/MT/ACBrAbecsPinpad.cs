using System;
using System.Globalization;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;
using ACBrLib.AbecsPinpad;
using System.Threading;

//MT
namespace ACBrLib.AbecsPinpad
{
    /// <inheritdoc />
    public sealed partial class ACBrAbecsPinpad : ACBrLibHandle
    {

        public ACBrAbecsPinpad(string eArqConfig = "", string eChaveCrypt = "") : base(IsWindows ? "ACBrAbecsPinpad64.dll" : "libacbrabecspinpad64.so",
                                                                                      IsWindows ? "ACBrAbecsPinpad32.dll" : "libacbrabecspinpad32.so")
        {
            var inicializar = GetMethod<AbecsPinpad_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ref libHandle, ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);

            Config = new AbecsPinpadConfig(this);
        }

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<AbecsPinpad_Nome>();
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

                var method = GetMethod<AbecsPinpad_Versao>();
                var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        public AbecsPinpadConfig Config { get; }


        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<AbecsPinpad_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig)
        {
            var lerIni = GetMethod<AbecsPinpad_ConfigImportar>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<AbecsPinpad_ConfigExportar>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<AbecsPinpad_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<AbecsPinpad_ConfigLerValor>();

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

            var method = GetMethod<AbecsPinpad_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        public void Ativar()
        {
            var method = GetMethod<AbecsPinpad_Ativar>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void Desativar()
        {
            var method = GetMethod<AbecsPinpad_Desativar>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void OPN()
        {
            var method = GetMethod<AbecsPinpad_OPN>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void CLO(string sMensagem = "")
        {
            var method = GetMethod<AbecsPinpad_CLO>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(sMensagem)));

            CheckResult(ret);
        }

        public void CLX(string sMensagemOuNomeImagem = "")
        {
            var method = GetMethod<AbecsPinpad_CLX>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(sMensagemOuNomeImagem)));

            CheckResult(ret);
        }

        public string GIX(string PP_DATA)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<AbecsPinpad_GIX>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(PP_DATA), buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string GIN(int GIN_ACQIDX)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<AbecsPinpad_GIN>();
            var ret = ExecuteMethod(() => method(libHandle, GIN_ACQIDX, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string PinPadCapabilities()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<AbecsPinpad_PinPadCapabilities>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void DSP(string sMensagem)
        {
            var method = GetMethod<AbecsPinpad_DSP>();
            var ret = ExecuteMethod(() => method(libHandle, sMensagem));

            CheckResult(ret);
        }

        public void DEX(string sMensagem)
        {
            var method = GetMethod<AbecsPinpad_DEX>();
            var ret = ExecuteMethod(() => method(libHandle, sMensagem));

            CheckResult(ret);
        }

        public void GKY()
        {
            var method = GetMethod<AbecsPinpad_GKY>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void RMC(string sMensagemRMC)
        {
            var method = GetMethod<AbecsPinpad_RMC>();
            var ret = ExecuteMethod(() => method(libHandle, sMensagemRMC));

            CheckResult(ret);
        }

        public string GCD(int aMSGIDX, int aTimeOut)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<AbecsPinpad_GCD>();
            var ret = ExecuteMethod(() => method(libHandle, aMSGIDX, aTimeOut, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string CEX(bool VerifyKey, bool VerifyMagnetic, bool VerifyICCInsertion, bool VerifyICCRemoval, bool VerifyCTLSPresence, int aTimeOut)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<AbecsPinpad_CEX>();
            var ret = ExecuteMethod(() => method(libHandle, VerifyKey, VerifyMagnetic, VerifyICCInsertion, VerifyICCRemoval, VerifyCTLSPresence, aTimeOut, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string MNU(string sMNUOPT, string sDSPMSG, int aTimeOut)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<AbecsPinpad_MNU>();
            var ret = ExecuteMethod(() => method(libHandle, sMNUOPT, sDSPMSG, aTimeOut, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string LoadMedia(string sCaminhoImagem, int aTipoImagem)
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<AbecsPinpad_LoadMedia>();
            var ret = ExecuteMethod(() => method(libHandle, sCaminhoImagem, aTipoImagem, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public string LMF()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<AbecsPinpad_LMF>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        public void DSI(string sNomeArquivo)
        {
            var method = GetMethod<AbecsPinpad_DSI>();
            var ret = ExecuteMethod(() => method(libHandle, sNomeArquivo));

            CheckResult(ret);
        }

        public void DMF(string sNomeArquivo)
        {
            var method = GetMethod<AbecsPinpad_DMF>();
            var ret = ExecuteMethod(() => method(libHandle, sNomeArquivo));

            CheckResult(ret);
        }

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<AbecsPinpad_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar(libHandle));
            CheckResult(codRet);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<AbecsPinpad_UltimoRetorno>();

            if (iniBufferLen < 1)
            {
                ExecuteMethod(() => ultimoRetorno(libHandle, buffer, ref bufferLen));
                if (bufferLen <= BUFFER_LEN) return FromUTF8(buffer);

                buffer.Capacity = bufferLen;
            }

            ExecuteMethod(() => ultimoRetorno(libHandle, buffer, ref bufferLen));
            return FromUTF8(buffer);
        }
    }
}