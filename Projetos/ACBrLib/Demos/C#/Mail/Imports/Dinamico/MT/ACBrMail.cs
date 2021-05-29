using System;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.Mail;

namespace ACBrLib.Mail
{
    public sealed partial class ACBrMail : ACBrLibHandle
    {
        #region Constructors

        public ACBrMail(string eArqConfig = "", string eChaveCrypt = "") : base(IsWindows ? "ACBrMail64.dll" : "libacbrmail64.so",
                                                                                IsWindows ? "ACBrMail32.dll" : "libacbrmail32.so")
        {
            var inicializar = GetMethod<MAIL_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ref libHandle, ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);
            Config = new MailConfig(this);
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get
            {
                var bufferLen = BUFFER_LEN;
                var buffer = new StringBuilder(bufferLen);

                var method = GetMethod<MAIL_Nome>();
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

                var method = GetMethod<MAIL_Versao>();
                var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

                CheckResult(ret);

                return ProcessResult(buffer, bufferLen);
            }
        }

        public MailConfig Config { get; }

        #endregion Properties

        #region Methods

        #region Ini

        public override void ConfigGravar(string eArqConfig = "")
        {
            var gravarIni = GetMethod<MAIL_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<MAIL_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<MAIL_ConfigLerValor>();

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

            var method = GetMethod<MAIL_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        public override void ImportarConfig(string eArqConfig = "")
        {
            var importarConfig = GetMethod<MAIL_ConfigImportar>();
            var ret = ExecuteMethod(() => importarConfig(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public override string ExportarConfig()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);

            var method = GetMethod<MAIL_ConfigExportar>();
            var ret = ExecuteMethod(() => method(libHandle, buffer, ref bufferLen));

            CheckResult(ret);

            return ProcessResult(buffer, bufferLen);
        }

        #endregion Ini

        public void SetSubject(string subject)
        {
            var method = GetMethod<MAIL_SetSubject>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(subject)));

            CheckResult(ret);
        }

        public void AddAddress(string eEmail, string eName)
        {
            var method = GetMethod<MAIL_AddAddress>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eEmail), ToUTF8(eName)));

            CheckResult(ret);
        }

        public void AddReplyTo(string eEmail, string eName)
        {
            var method = GetMethod<MAIL_AddReplyTo>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eEmail), ToUTF8(eName)));

            CheckResult(ret);
        }

        public void AddCC(string eEmail, string eName)
        {
            var method = GetMethod<MAIL_AddCC>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eEmail), ToUTF8(eName)));

            CheckResult(ret);
        }

        public void AddBCC(string eEmail, string eName)
        {
            var method = GetMethod<MAIL_AddBCC>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eEmail)));

            CheckResult(ret);
        }

        public void AddAttachment(string eFileName, string eDescription, MailAttachmentDisposition aDisposition)
        {
            var method = GetMethod<MAIL_AddAttachment>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eFileName), ToUTF8(eDescription), (int)aDisposition));

            CheckResult(ret);
        }

        public void ClearAttachment()
        {
            var method = GetMethod<MAIL_ClearAttachment>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void AddBody(string eBody)
        {
            var method = GetMethod<MAIL_AddBody>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eBody)));

            CheckResult(ret);
        }

        public void AddAltBody(string eAltBody)
        {
            var method = GetMethod<MAIL_AddAltBody>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eAltBody)));

            CheckResult(ret);
        }

        public void SaveToFile(string eFileName)
        {
            var method = GetMethod<MAIL_SaveToFile>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eFileName)));

            CheckResult(ret);
        }

        public void Clear()
        {
            var method = GetMethod<MAIL_Clear>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void Send()
        {
            var method = GetMethod<MAIL_Send>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<MAIL_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar(libHandle));
            CheckResult(codRet);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<MAIL_UltimoRetorno>();

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