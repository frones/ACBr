using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLib.BAL
{
    public sealed partial class ACBrBAL
    {
        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_Inicializar(string eArqConfig, string eChaveCrypt);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_Finalizar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_Nome(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_Versao(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_UltimoRetorno(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_ConfigImportar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_ConfigExportar(StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_ConfigLer(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_ConfigGravar(string eArqConfig);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_ConfigLerValor(string eSessao, string eChave, StringBuilder buffer, ref int bufferSize);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_ConfigGravarValor(string eSessao, string eChave, string valor);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_Ativar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_Desativar();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_LePeso(int MillisecTimeOut, ref double peso);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_SolicitarPeso();

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_UltimoPesoLido(ref double peso);

        [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
        public delegate int BAL_InterpretarRespostaPeso(string resposta, ref double peso);

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