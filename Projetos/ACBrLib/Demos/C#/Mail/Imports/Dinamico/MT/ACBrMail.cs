using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;
using ACBrLib.Core;
using ACBrLib.Core.Mail;

namespace ACBrLib.Mail
{
    public sealed class ACBrMail : ACBrLibHandle
    {
        #region InnerTypes

        private class Delegates
        {
            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_AddAddress(IntPtr handle, string eEmail, string eName);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_AddAltBody(IntPtr handle, string eAltBody);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_AddAttachment(IntPtr handle, string eFileName, string eDescription, int aDisposition);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_AddBCC(IntPtr handle, string eEmail);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_AddBody(IntPtr handle, string eBody);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_AddCC(IntPtr handle, string eEmail, string eName);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_AddReplyTo(IntPtr handle, string eEmail, string eName);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_Clear(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_ClearAttachment(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_ConfigGravar(IntPtr handle, string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_ConfigLer(IntPtr handle, string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer,
                ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_Finalizar(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_SaveToFile(IntPtr handle, string eFileName);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_Send(IntPtr handle);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_SetSubject(IntPtr handle, string eSubject);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);
        }

        #endregion InnerTypes

        #region Constructors

        public ACBrMail(string eArqConfig = "", string eChaveCrypt = "") :
            base(Environment.Is64BitProcess ? "ACBrMail64.dll" : "ACBrMail32.dll")
        {
            var inicializar = GetMethod<Delegates.MAIL_Inicializar>();
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

                var method = GetMethod<Delegates.MAIL_Nome>();
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

                var method = GetMethod<Delegates.MAIL_Versao>();
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
            var gravarIni = GetMethod<Delegates.MAIL_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public void ConfigLer(string eArqConfig = "")
        {
            var lerIni = GetMethod<Delegates.MAIL_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(libHandle, ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<Delegates.MAIL_ConfigLerValor>();

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

            var method = GetMethod<Delegates.MAIL_ConfigGravarValor>();
            var propValue = ConvertValue(value);

            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        public void SetSubject(string subject)
        {
            var method = GetMethod<Delegates.MAIL_SetSubject>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(subject)));

            CheckResult(ret);
        }

        public void AddAddress(string eEmail, string eName)
        {
            var method = GetMethod<Delegates.MAIL_AddAddress>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eEmail), ToUTF8(eName)));

            CheckResult(ret);
        }

        public void AddReplyTo(string eEmail, string eName)
        {
            var method = GetMethod<Delegates.MAIL_AddReplyTo>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eEmail), ToUTF8(eName)));

            CheckResult(ret);
        }

        public void AddCC(string eEmail, string eName)
        {
            var method = GetMethod<Delegates.MAIL_AddCC>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eEmail), ToUTF8(eName)));

            CheckResult(ret);
        }

        public void AddBCC(string eEmail, string eName)
        {
            var method = GetMethod<Delegates.MAIL_AddBCC>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eEmail)));

            CheckResult(ret);
        }

        public void AddAttachment(string eFileName, string eDescription, MailAttachmentDisposition aDisposition)
        {
            var method = GetMethod<Delegates.MAIL_AddAttachment>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eFileName), ToUTF8(eDescription), (int)aDisposition));

            CheckResult(ret);
        }

        public void ClearAttachment()
        {
            var method = GetMethod<Delegates.MAIL_ClearAttachment>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void AddBody(string eBody)
        {
            var method = GetMethod<Delegates.MAIL_AddBody>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eBody)));

            CheckResult(ret);
        }

        public void AddAltBody(string eAltBody)
        {
            var method = GetMethod<Delegates.MAIL_AddAltBody>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eAltBody)));

            CheckResult(ret);
        }

        public void SaveToFile(string eFileName)
        {
            var method = GetMethod<Delegates.MAIL_SaveToFile>();
            var ret = ExecuteMethod(() => method(libHandle, ToUTF8(eFileName)));

            CheckResult(ret);
        }

        public void Clear()
        {
            var method = GetMethod<Delegates.MAIL_Clear>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        public void Send()
        {
            var method = GetMethod<Delegates.MAIL_Send>();
            var ret = ExecuteMethod(() => method(libHandle));

            CheckResult(ret);
        }

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<Delegates.MAIL_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar(libHandle));
            CheckResult(codRet);
        }

        protected override string GetUltimoRetorno(int iniBufferLen = 0)
        {
            var bufferLen = iniBufferLen < 1 ? BUFFER_LEN : iniBufferLen;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<Delegates.MAIL_UltimoRetorno>();

            if (iniBufferLen < 1)
            {
                ExecuteMethod(() => ultimoRetorno(libHandle, buffer, ref bufferLen));
                if (bufferLen <= BUFFER_LEN) return FromUTF8(buffer);

                buffer.Capacity = bufferLen;
            }

            ExecuteMethod(() => ultimoRetorno(libHandle, buffer, ref bufferLen));
            return FromUTF8(buffer);
        }

        protected override void InitializeMethods()
        {
            AddMethod<Delegates.MAIL_Inicializar>("MAIL_Inicializar");
            AddMethod<Delegates.MAIL_Finalizar>("MAIL_Finalizar");
            AddMethod<Delegates.MAIL_Nome>("MAIL_Nome");
            AddMethod<Delegates.MAIL_Versao>("MAIL_Versao");
            AddMethod<Delegates.MAIL_UltimoRetorno>("MAIL_UltimoRetorno");
            AddMethod<Delegates.MAIL_ConfigLer>("MAIL_ConfigLer");
            AddMethod<Delegates.MAIL_ConfigGravar>("MAIL_ConfigGravar");
            AddMethod<Delegates.MAIL_ConfigLerValor>("MAIL_ConfigLerValor");
            AddMethod<Delegates.MAIL_ConfigGravarValor>("MAIL_ConfigGravarValor");
            AddMethod<Delegates.MAIL_SetSubject>("MAIL_SetSubject");
            AddMethod<Delegates.MAIL_AddAddress>("MAIL_AddAddress");
            AddMethod<Delegates.MAIL_AddReplyTo>("MAIL_AddReplyTo");
            AddMethod<Delegates.MAIL_AddCC>("MAIL_AddCC");
            AddMethod<Delegates.MAIL_AddBCC>("MAIL_AddBCC");
            AddMethod<Delegates.MAIL_AddAttachment>("MAIL_AddAttachment");
            AddMethod<Delegates.MAIL_ClearAttachment>("MAIL_ClearAttachment");
            AddMethod<Delegates.MAIL_AddBody>("MAIL_AddBody");
            AddMethod<Delegates.MAIL_AddAltBody>("MAIL_AddAltBody");
            AddMethod<Delegates.MAIL_SaveToFile>("MAIL_SaveToFile");
            AddMethod<Delegates.MAIL_Clear>("MAIL_Clear");
            AddMethod<Delegates.MAIL_Send>("MAIL_Send");
        }

        #endregion Private Methods

        #endregion Methods
    }
}