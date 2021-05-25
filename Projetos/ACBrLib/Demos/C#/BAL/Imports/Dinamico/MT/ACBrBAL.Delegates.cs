using System;
using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLib.BAL
{
    public sealed partial class ACBrBAL
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_Inicializar(ref IntPtr handle, string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_Finalizar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_Nome(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_Versao(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_UltimoRetorno(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_ConfigImportar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_ConfigExportar(IntPtr handle, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_ConfigLer(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_ConfigGravar(IntPtr handle, string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_ConfigLerValor(IntPtr handle, string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_ConfigGravarValor(IntPtr handle, string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_Ativar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_Desativar(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_LePeso(IntPtr handle, int MillisecTimeOut, ref double peso);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_SolicitarPeso(IntPtr handle);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_UltimoPesoLido(IntPtr handle, ref double peso);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_InterpretarRespostaPeso(IntPtr handle, string resposta, ref double peso);

        protected override void InitializeMethods()
        {
            AddMethod<BAL_Inicializar>("BAL_Inicializar");
            AddMethod<BAL_Finalizar>("BAL_Finalizar");
            AddMethod<BAL_Nome>("BAL_Nome");
            AddMethod<BAL_Versao>("BAL_Versao");
            AddMethod<BAL_UltimoRetorno>("BAL_UltimoRetorno");
            AddMethod<BAL_ConfigImportar>("BAL_ConfigImportar");
            AddMethod<BAL_ConfigExportar>("BAL_ConfigExportar");
            AddMethod<BAL_ConfigLer>("BAL_ConfigLer");
            AddMethod<BAL_ConfigGravar>("BAL_ConfigGravar");
            AddMethod<BAL_ConfigLerValor>("BAL_ConfigLerValor");
            AddMethod<BAL_ConfigGravarValor>("BAL_ConfigGravarValor");
            AddMethod<BAL_Ativar>("BAL_Ativar");
            AddMethod<BAL_Desativar>("BAL_Desativar");
            AddMethod<BAL_LePeso>("BAL_LePeso");
            AddMethod<BAL_SolicitarPeso>("BAL_SolicitarPeso");
            AddMethod<BAL_UltimoPesoLido>("BAL_UltimoPesoLido");
            AddMethod<BAL_InterpretarRespostaPeso>("BAL_InterpretarRespostaPeso");
        }
    }
}