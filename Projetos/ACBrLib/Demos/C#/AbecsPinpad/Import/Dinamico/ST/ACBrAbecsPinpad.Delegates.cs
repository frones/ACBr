using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace ACBrLib.AbecsPinpad
{
    public sealed partial class ACBrAbecsPinpad
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_Ativar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_Desativar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_OPN();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_CLO(string sMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_CLX(string sMensagemOuNomeImagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_GIX(string PP_DATA, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_GIN(int GIN_ACQIDX, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_PinPadCapabilities(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_DSP(string sMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_DEX(string sMensagem);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_GKY();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_RMC(string sMensagemRMC);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_GCD(int aMSGIDX, int aTimeOut, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_CEX(bool VerifyKey, bool VerifyMagnetic, bool VerifyICCInsertion, bool VerifyICCRemoval, bool VerifyCTLSPresence, int aTimeOut, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_MNU(string sMNUOPT, string sDSPMSG, int aTimeOut, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_LoadMedia(string sCaminhoImagem, int aTipoImagem, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_LMF(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_DSI(string sNomeArquivo);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int AbecsPinpad_DMF(string sNomeArquivo);

        protected override void InitializeMethods()
        {
            AddMethod<AbecsPinpad_Inicializar>("AbecsPinpad_Inicializar");
            AddMethod<AbecsPinpad_Finalizar>("AbecsPinpad_Finalizar");
            AddMethod<AbecsPinpad_Nome>("AbecsPinpad_Nome");
            AddMethod<AbecsPinpad_Versao>("AbecsPinpad_Versao");
            AddMethod<AbecsPinpad_UltimoRetorno>("AbecsPinpad_UltimoRetorno");
            AddMethod<AbecsPinpad_ConfigImportar>("AbecsPinpad_ConfigImportar");
            AddMethod<AbecsPinpad_ConfigExportar>("AbecsPinpad_ConfigExportar");
            AddMethod<AbecsPinpad_ConfigLer>("AbecsPinpad_ConfigLer");
            AddMethod<AbecsPinpad_ConfigGravar>("AbecsPinpad_ConfigGravar");
            AddMethod<AbecsPinpad_ConfigLerValor>("AbecsPinpad_ConfigLerValor");
            AddMethod<AbecsPinpad_ConfigGravarValor>("AbecsPinpad_ConfigGravarValor");
            AddMethod<AbecsPinpad_Ativar>("AbecsPinpad_Ativar");
            AddMethod<AbecsPinpad_Desativar>("AbecsPinpad_Desativar");
            AddMethod<AbecsPinpad_OPN>("AbecsPinpad_OPN");
            AddMethod<AbecsPinpad_CLO>("AbecsPinpad_CLO");
            AddMethod<AbecsPinpad_CLX>("AbecsPinpad_CLX");
            AddMethod<AbecsPinpad_GIX>("AbecsPinpad_GIX");
            AddMethod<AbecsPinpad_GIN>("AbecsPinpad_GIN");
            AddMethod<AbecsPinpad_PinPadCapabilities>("AbecsPinpad_PinPadCapabilities");
            AddMethod<AbecsPinpad_DSP>("AbecsPinpad_DSP");
            AddMethod<AbecsPinpad_DEX>("AbecsPinpad_DEX");
            AddMethod<AbecsPinpad_GKY>("AbecsPinpad_GKY");
            AddMethod<AbecsPinpad_RMC>("AbecsPinpad_RMC");
            AddMethod<AbecsPinpad_GCD>("AbecsPinpad_GCD");
            AddMethod<AbecsPinpad_CEX>("AbecsPinpad_CEX");
            AddMethod<AbecsPinpad_MNU>("AbecsPinpad_MNU");
            AddMethod<AbecsPinpad_LoadMedia>("AbecsPinpad_LoadMedia");
            AddMethod<AbecsPinpad_LMF>("AbecsPinpad_LMF");
            AddMethod<AbecsPinpad_DSI>("AbecsPinpad_DSI");
            AddMethod<AbecsPinpad_DMF>("AbecsPinpad_DMF");
        }
    }
}