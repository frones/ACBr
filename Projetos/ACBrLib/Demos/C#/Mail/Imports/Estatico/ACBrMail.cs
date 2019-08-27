using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLibMail
{
    public static class ACBrMail
    {
        #region DLL

        private const string ACBr = "ACBrMail32.dll";
        //private const string ACBr = "ACBrMail64.dll";

        #endregion DLL

        #region Imports

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_Inicializar(string eArqConfig, string eChaveCrypt);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_Finalizar();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_Nome(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_Versao(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_ConfigLer(string eArqConfig);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_ConfigGravar(string eArqConfig);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_ConfigGravarValor(string eSessao, string eChave, string valor);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_SetSubject(string eSubject);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_AddAddress(string eEmail, string eName);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_AddReplyTo(string eEmail, string eName);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_AddCC(string eEmail, string eName);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_AddBCC(string eEmail);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_AddAttachment(string eFileName, string eDescription, int aDisposition);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_ClearAttachment();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_AddBody(string eBody);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_AddAltBody(string eAltBody);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_SaveToFile(string eFileName);

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_Clear();

        [DllImport(ACBr, CallingConvention = CallingConvention.Cdecl)]
        public static extern int MAIL_Send();

        #endregion Imports

        #region Methods

        public static string ToUTF8(this string value)
        {
            return string.IsNullOrEmpty(value) ? value : Encoding.Default.GetString(Encoding.UTF8.GetBytes(value));
        }

        public static string FromUTF8(this StringBuilder value)
        {
            if (value == null) return null;
            return value.Length == 0 ? string.Empty : Encoding.UTF8.GetString(Encoding.Default.GetBytes(value.ToString()));
        }

        public static void CheckResult(int ret)
        {
            if (ret >= 0) return;

            var bufferLen = 256;
            var buffer = new StringBuilder(bufferLen);

            MAIL_UltimoRetorno(buffer, ref bufferLen);
            if (bufferLen > 256)
            {
                buffer.Capacity = bufferLen;
                MAIL_UltimoRetorno(buffer, ref bufferLen);
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

        #endregion Methods
    }
}