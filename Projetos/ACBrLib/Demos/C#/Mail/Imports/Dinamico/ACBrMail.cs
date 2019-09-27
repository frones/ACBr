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
            public delegate int MAIL_AddAddress(string eEmail, string eName);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_AddAltBody(string eAltBody);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_AddAttachment(string eFileName, string eDescription, int aDisposition);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_AddBCC(string eEmail);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_AddBody(string eBody);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_AddCC(string eEmail, string eName);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_AddReplyTo(string eEmail, string eName);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_Clear();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_ClearAttachment();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_ConfigGravar(string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_ConfigGravarValor(string eSessao, string eChave, string valor);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_ConfigLer(string eArqConfig);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer,
                ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_Finalizar();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_Inicializar(string eArqConfig, string eChaveCrypt);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_Nome(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_SaveToFile(string eFileName);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_Send();

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_SetSubject(string eSubject);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

            [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
            public delegate int MAIL_Versao(StringBuilder buffer, ref int bufferSize);
        }

        #endregion InnerTypes

        #region Constructors

        public ACBrMail(string eArqConfig = "", string eChaveCrypt = "") :
            base(Environment.Is64BitProcess ? "ACBrMail64.dll" : "ACBrMail32.dll")
        {
            InitializeMethods();

            var inicializar = GetMethod<Delegates.MAIL_Inicializar>();
            var ret = ExecuteMethod(() => inicializar(ToUTF8(eArqConfig), ToUTF8(eChaveCrypt)));

            CheckResult(ret);
        }

        #endregion Constructors

        #region Methods

        #region Ini

        public void ConfigGravar(string eArqConfig = "ACBrLib.ini")
        {
            var gravarIni = GetMethod<Delegates.MAIL_ConfigGravar>();
            var ret = ExecuteMethod(() => gravarIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public void ConfigLer(string eArqConfig = "ACBrLib.ini")
        {
            var lerIni = GetMethod<Delegates.MAIL_ConfigLer>();
            var ret = ExecuteMethod(() => lerIni(ToUTF8(eArqConfig)));

            CheckResult(ret);
        }

        public T ConfigLerValor<T>(ACBrSessao eSessao, string eChave)
        {
            var method = GetMethod<Delegates.MAIL_ConfigLerValor>();

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

            var method = GetMethod<Delegates.MAIL_ConfigGravarValor>();
            var type = value.GetType();

            var propValue = value.ToString();
            if (type.IsEnum) propValue = ((int)value).ToString();
            if (type == typeof(bool)) propValue = Convert.ToInt32(value).ToString();

            var ret = ExecuteMethod(() => method(ToUTF8(eSessao.ToString()), ToUTF8(eChave), ToUTF8(propValue)));
            CheckResult(ret);
        }

        #endregion Ini

        public void SetSubject(string subject)
        {
            var method = GetMethod<Delegates.MAIL_SetSubject>();
            var ret = ExecuteMethod(() => method(ToUTF8(subject)));

            CheckResult(ret);
        }

        public void AddAddress(string eEmail, string eName)
        {
            var method = GetMethod<Delegates.MAIL_AddAddress>();
            var ret = ExecuteMethod(() => method(ToUTF8(eEmail), ToUTF8(eName)));

            CheckResult(ret);
        }

        public void AddReplyTo(string eEmail, string eName)
        {
            var method = GetMethod<Delegates.MAIL_AddReplyTo>();
            var ret = ExecuteMethod(() => method(ToUTF8(eEmail), ToUTF8(eName)));

            CheckResult(ret);
        }

        public void AddCC(string eEmail, string eName)
        {
            var method = GetMethod<Delegates.MAIL_AddCC>();
            var ret = ExecuteMethod(() => method(ToUTF8(eEmail), ToUTF8(eName)));

            CheckResult(ret);
        }

        public void AddBCC(string eEmail, string eName)
        {
            var method = GetMethod<Delegates.MAIL_AddBCC>();
            var ret = ExecuteMethod(() => method(ToUTF8(eEmail)));

            CheckResult(ret);
        }

        public void AddAttachment(string eFileName, string eDescription, MailAttachmentDisposition aDisposition)
        {
            var method = GetMethod<Delegates.MAIL_AddAttachment>();
            var ret = ExecuteMethod(() => method(ToUTF8(eFileName), ToUTF8(eDescription), (int)aDisposition));

            CheckResult(ret);
        }

        public void ClearAttachment()
        {
            var method = GetMethod<Delegates.MAIL_ClearAttachment>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void AddBody(string eBody)
        {
            var method = GetMethod<Delegates.MAIL_AddBody>();
            var ret = ExecuteMethod(() => method(ToUTF8(eBody)));

            CheckResult(ret);
        }

        public void AddAltBody(string eAltBody)
        {
            var method = GetMethod<Delegates.MAIL_AddAltBody>();
            var ret = ExecuteMethod(() => method(ToUTF8(eAltBody)));

            CheckResult(ret);
        }

        public void SaveToFile(string eFileName)
        {
            var method = GetMethod<Delegates.MAIL_SaveToFile>();
            var ret = ExecuteMethod(() => method(ToUTF8(eFileName)));

            CheckResult(ret);
        }

        public void Clear()
        {
            var method = GetMethod<Delegates.MAIL_Clear>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        public void Send()
        {
            var method = GetMethod<Delegates.MAIL_Send>();
            var ret = ExecuteMethod(() => method());

            CheckResult(ret);
        }

        #region Private Methods

        protected override void FinalizeLib()
        {
            var finalizar = GetMethod<Delegates.MAIL_Finalizar>();
            var codRet = ExecuteMethod(() => finalizar());
            CheckResult(codRet);
        }

        protected override string GetUltimoRetorno()
        {
            var bufferLen = BUFFER_LEN;
            var buffer = new StringBuilder(bufferLen);
            var ultimoRetorno = GetMethod<Delegates.MAIL_UltimoRetorno>();

            ExecuteMethod(() => ultimoRetorno(buffer, ref bufferLen));

            if (bufferLen <= BUFFER_LEN) return FromUTF8(buffer);

            buffer.Capacity = bufferLen;
            ExecuteMethod(() => ultimoRetorno(buffer, ref bufferLen));

            return FromUTF8(buffer);
        }

        private void InitializeMethods()
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