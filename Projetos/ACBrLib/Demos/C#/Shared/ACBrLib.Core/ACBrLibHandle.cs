using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.ConstrainedExecution;
using System.Runtime.ExceptionServices;
using System.Runtime.InteropServices;
using System.Text;

namespace ACBrLib.Core
{
    /// <inheritdoc />
    public abstract partial class ACBrLibHandle : SafeHandle
    {
        #region Fields

        protected readonly Dictionary<Type, string> methodList;
        protected readonly Dictionary<string, Delegate> methodCache;
        protected readonly string className;
        protected const int BUFFER_LEN = 256;
        protected IntPtr libHandle;
        private static string libraryPath;

        #endregion Fields

        #region Constructors

        static ACBrLibHandle()
        {
            MinusOne = new IntPtr(-1);

            var baseDir = AppDomain.CurrentDomain.BaseDirectory;
            if (!string.IsNullOrEmpty(baseDir))
            {
                var uri = new Uri(baseDir);
                var path = Path.GetDirectoryName(!uri.IsFile ? uri.ToString() : uri.LocalPath + Uri.UnescapeDataString(uri.Fragment));
                LibraryPath = Path.Combine(path, "ACBrLib", Environment.Is64BitProcess ? "x64" : "x86");
            }
        }

        protected ACBrLibHandle(string dllName64, string dllName32) :
            this(Environment.Is64BitProcess ? dllName64 : dllName32)
        {
        }

        protected ACBrLibHandle(string dllName) : base(IntPtr.Zero, true)
        {
            methodCache = new Dictionary<string, Delegate>();
            methodList = new Dictionary<Type, string>();
            className = GetType().Name;

            var pNewSession = LibLoader.LoadLibrary(dllName);
            if (pNewSession == IntPtr.Zero || pNewSession == MinusOne)
            {
                pNewSession = LibLoader.LoadLibrary(Path.Combine(LibraryPath, dllName));

                // Localizar dependências em pacotes nuget
                if (pNewSession == IntPtr.Zero || pNewSession == MinusOne)
                {
                    DirectoryInfo directoryInfo;
                    string pastaPackage = "";
                    string libraryNuget = "";
                    string libraryPathParent = LibraryPath;

                    // Navega nas pastas anteriores até localizar a pasta onde está a Packages
                    while (Directory.GetParent(libraryPathParent) != null)
                    {
                        if (String.IsNullOrEmpty(libraryPathParent))
                            break;

                        directoryInfo = Directory.GetParent(libraryPathParent);

                        if (directoryInfo == null)
                            break;

                        libraryPathParent = directoryInfo.FullName;

                        if (Directory.Exists(libraryPathParent))
                        {
                            libraryNuget = libraryPathParent;
                            string[] pastas = Directory.GetDirectories(libraryNuget);

                            // Varre o caminho para verificar se existe a pasta Packages onde os nugets distribuem as dependências
                            foreach (string pasta in pastas)
                            {
                                pastaPackage = Path.GetFileName(pasta);

                                if (pastaPackage.ToLower() == "packages")
                                    break;

                                pastaPackage = "";
                            }

                            if (!String.IsNullOrEmpty(pastaPackage))
                            {
                                libraryNuget = Path.Combine(libraryNuget, pastaPackage);

                                pastas = Directory.GetDirectories(libraryNuget);
                                pastaPackage = "";

                                // Varre a pasta Packages para localizar a pasta da lib
                                foreach (string dir in pastas)
                                {
                                    pastaPackage = Path.GetFileName(dir);

                                    if (pastaPackage.Substring(0, 8).ToLower() == "acbrlib.")
                                        break;

                                    pastaPackage = "";
                                }

                                // Ao locaizar a pasta de lib completa o caminho com a arquitetura da aplicação para pegar as dependências
                                if (!String.IsNullOrEmpty(pastaPackage))
                                {
                                    // Concatena a pasta do pacote
                                    libraryNuget = Path.Combine(libraryNuget, pastaPackage, "ACBrLib");

                                    // Concatena a pasta das dependências
                                    libraryNuget = Path.Combine(libraryNuget, Environment.Is64BitProcess ? "x64" : "x86");

                                    if (Directory.Exists(libraryNuget))
                                        break;
                                }
                            }
                        }
                    }

                    if (!String.IsNullOrEmpty(libraryNuget))
                        pNewSession = LibLoader.LoadLibrary(Path.Combine(libraryNuget, dllName));

                    if (pNewSession == IntPtr.Zero || pNewSession == MinusOne)
                        throw CreateException("Não foi possivel carregar a biblioteca na pasta da aplicação ou no caminho padrão: " + LibraryPath);

                    LibraryPath = libraryNuget;
                }
            }

            SetHandle(pNewSession);
            InitializeMethods();
        }

        #endregion Constructors

        #region Properties

        public static string LibraryPath
        {
            get => libraryPath;
            set
            {
                if (value != libraryPath)
                    Environment.SetEnvironmentVariable("PATH", value);

                libraryPath = value;
            }
        }

        /// <summary>
        /// </summary>
        protected static IntPtr MinusOne { get; }

        /// <inheritdoc />
        public override bool IsInvalid
        {
            get
            {
                if (handle != IntPtr.Zero) return handle == MinusOne;

                return true;
            }
        }

        public static bool IsWindows => LibLoader.IsWindows;

        public static bool IsOSX => LibLoader.IsOSX;

        public static bool IsLinux => !LibLoader.IsOSX && !LibLoader.IsWindows;

        #endregion Properties

        #region Methods

        public abstract void ConfigGravar(string eArqConfig = "");

        public abstract void ConfigLer(string eArqConfig = "");

        public abstract T ConfigLerValor<T>(ACBrSessao eSessao, string eChave);

        public abstract void ConfigGravarValor(ACBrSessao eSessao, string eChave, object value);

        public abstract void ImportarConfig(string eArqConfig);

        public abstract string ExportarConfig();

        protected async void Base64ToStream(string base64, Stream aStream)
        {
            var pdfBytes = Convert.FromBase64String(base64);
            await aStream.WriteAsync(pdfBytes, 0, pdfBytes.Length);
            await aStream.FlushAsync();

            aStream.Position = 0;
        }

        /// <inheritdoc />
        [ReliabilityContract(Consistency.WillNotCorruptState, Cer.Success)]
        protected override bool ReleaseHandle()
        {
            if (IsInvalid) return true;

            FinalizeLib();

            var ret = LibLoader.FreeLibrary(handle);

            if (ret)
                SetHandleAsInvalid();

            return ret;
        }

        protected abstract void InitializeMethods();

        protected abstract string GetUltimoRetorno(int iniBufferLen = 0);

        protected abstract void FinalizeLib();

        protected virtual T ConvertValue<T>(string value)
        {
            if (typeof(T).IsEnum && !Attribute.IsDefined(typeof(T), typeof(FlagsAttribute))) return (T)Enum.ToObject(typeof(T), Convert.ToInt32(value));
            if (typeof(T).IsEnum && Attribute.IsDefined(typeof(T), typeof(FlagsAttribute))) return (T)Enum.Parse(typeof(T), value.Trim('[', ']'), true);
            if (typeof(T) == typeof(bool)) return (T)(object)Convert.ToBoolean(Convert.ToInt32(value));
            if (typeof(T) == typeof(byte[])) return (T)(object)Convert.FromBase64String(value);
            if (typeof(T) != typeof(Stream)) return (T)Convert.ChangeType(value, typeof(T));

            var dados = Convert.FromBase64String(value);
            var ms = new MemoryStream();
            ms.Write(dados, 0, dados.Length);
            return (T)(object)ms;
        }

        protected virtual string ConvertValue(object value)
        {
            var type = value.GetType();
            var propValue = value.ToString();
            if (type.IsEnum && !Attribute.IsDefined(type, typeof(FlagsAttribute))) propValue = ((int)value).ToString();
            if (type.IsEnum && Attribute.IsDefined(type, typeof(FlagsAttribute))) propValue = $"[{Enum.Format(type, value, "F")}]";
            if (type == typeof(bool)) propValue = Convert.ToInt32(value).ToString();
            if (type == typeof(byte[])) propValue = Convert.ToBase64String((byte[])value);
            if (type != typeof(Stream)) return propValue;

            using (var ms = new MemoryStream())
            {
                ((Stream)value).CopyTo(ms);
                propValue = Convert.ToBase64String(ms.ToArray());
            }

            return propValue;
        }

        /// <summary>
        ///     Adiciona um delegate a lista para a função informada.
        /// </summary>
        /// <param name="functionName">Nome da função para exportar</param>
        /// <typeparam name="T">Delegate da função</typeparam>
        protected virtual void AddMethod<T>(string functionName) where T : class => methodList.Add(typeof(T), functionName);

        /// <summary>
        ///     Retorna o delegate para uso.
        /// </summary>
        /// <typeparam name="T">Delegate</typeparam>
        /// <returns></returns>
        /// <exception cref="ArgumentNullException"></exception>
        protected virtual T GetMethod<T>() where T : class
        {
            if (!methodList.ContainsKey(typeof(T)))
                throw CreateException($"Função não adicionada para o [{nameof(T)}].");

            var method = methodList[typeof(T)];
            if (methodCache.ContainsKey(method)) return methodCache[method] as T;

            var mHandler = LibLoader.GetProcAddress(handle, method);

            if (mHandler == IntPtr.Zero || mHandler == MinusOne)
                throw new ArgumentNullException("Função não encontrada: " + method);

            var methodHandler = LibLoader.LoadFunction<T>(mHandler);

            methodCache.Add(method, methodHandler as Delegate);
            return methodHandler;
        }

        /// <summary>
        ///     Executa a função e trata erros nativos.
        /// </summary>
        /// <param name="method"></param>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        /// <exception cref="ApplicationException"></exception>
        [HandleProcessCorruptedStateExceptions]
        protected virtual T ExecuteMethod<T>(Func<T> method)
        {
            try
            {
                return method();
            }
            catch (Exception exception)
            {
                throw ProcessException(exception);
            }
        }

        /// <summary>
        ///     Cria e dispara uma <see cref="ApplicationException" /> com a mensagem informada.
        /// </summary>
        /// <param name="errorMessage">Mensagem de erro.</param>
        /// <returns>
        ///     <see cref="ApplicationException" />
        /// </returns>
        protected virtual ApplicationException CreateException(string errorMessage) => new ApplicationException(errorMessage);

        /// <summary>
        ///     Tatar uma <see cref="Exception" /> e dispara uma <see cref="ApplicationException" /> com a mensagem da mesma.
        /// </summary>
        /// <param name="exception">Exception</param>
        /// <returns>
        ///     <see cref="ApplicationException" />
        /// </returns>
        protected virtual ApplicationException ProcessException(Exception exception) => new ApplicationException(exception.Message, exception);

        protected static string ToUTF8(string value) => string.IsNullOrEmpty(value) ? value : Encoding.Default.GetString(Encoding.UTF8.GetBytes(value));

        protected static string FromUTF8(StringBuilder value)
        {
            if (value == null) return null;
            var ret = value.ToString();

            return ret.Length == 0
                ? string.Empty
                : Encoding.UTF8.GetString(Encoding.Default.GetBytes(ret));
        }

        protected string ProcessResult(StringBuilder buffer, int bufferLen) => bufferLen > BUFFER_LEN ? GetUltimoRetorno(bufferLen) : FromUTF8(buffer);

        protected virtual void CheckResult(int ret)
        {
            if (ret >= 0) return;

            var message = GetUltimoRetorno();

            switch (ret)
            {
                case -6:
                    throw new DirectoryNotFoundException(message);

                case -5:
                    throw new FileNotFoundException(message);

                default:
                    throw new ApplicationException(message);
            }
        }

        #endregion Methods
    }
}