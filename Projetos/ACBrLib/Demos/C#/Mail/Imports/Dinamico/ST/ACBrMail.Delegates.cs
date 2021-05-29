using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace ACBrLib.Mail
{
    public sealed partial class ACBrMail
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
        public delegate int MAIL_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MAIL_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int MAIL_Versao(StringBuilder buffer, ref int bufferSize);

        protected override void InitializeMethods()
        {
            AddMethod<MAIL_Inicializar>("MAIL_Inicializar");
            AddMethod<MAIL_Finalizar>("MAIL_Finalizar");
            AddMethod<MAIL_Nome>("MAIL_Nome");
            AddMethod<MAIL_Versao>("MAIL_Versao");
            AddMethod<MAIL_UltimoRetorno>("MAIL_UltimoRetorno");
            AddMethod<MAIL_ConfigImportar>("MAIL_ConfigImportar");
            AddMethod<MAIL_ConfigExportar>("MAIL_ConfigExportar");
            AddMethod<MAIL_ConfigLer>("MAIL_ConfigLer");
            AddMethod<MAIL_ConfigGravar>("MAIL_ConfigGravar");
            AddMethod<MAIL_ConfigLerValor>("MAIL_ConfigLerValor");
            AddMethod<MAIL_ConfigGravarValor>("MAIL_ConfigGravarValor");
            AddMethod<MAIL_SetSubject>("MAIL_SetSubject");
            AddMethod<MAIL_AddAddress>("MAIL_AddAddress");
            AddMethod<MAIL_AddReplyTo>("MAIL_AddReplyTo");
            AddMethod<MAIL_AddCC>("MAIL_AddCC");
            AddMethod<MAIL_AddBCC>("MAIL_AddBCC");
            AddMethod<MAIL_AddAttachment>("MAIL_AddAttachment");
            AddMethod<MAIL_ClearAttachment>("MAIL_ClearAttachment");
            AddMethod<MAIL_AddBody>("MAIL_AddBody");
            AddMethod<MAIL_AddAltBody>("MAIL_AddAltBody");
            AddMethod<MAIL_SaveToFile>("MAIL_SaveToFile");
            AddMethod<MAIL_Clear>("MAIL_Clear");
            AddMethod<MAIL_Send>("MAIL_Send");
        }
    }
}