using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;

namespace ACBrLib.Core
{
    public sealed class ACBrIniFile : IEnumerable<ACBrIniSection>
    {
        #region Fields

        private List<ACBrIniSection> sections;

        #endregion Fields

        #region Constructors

        public ACBrIniFile() : this("", "")
        {
        }

        public ACBrIniFile(string iniFilePath, string iniFileName) : this(iniFilePath, iniFileName, Encoding.GetEncoding("ISO-8859-1"), 1024)
        {
        }

        public ACBrIniFile(string iniFilePath, string iniFileName, Encoding encoding, int bufferSize)
        {
            sections = new List<ACBrIniSection>();
            Encoding = encoding;
            BufferSize = bufferSize;
            IniFilePath = iniFilePath;
            IniFileName = iniFileName;
        }

        #endregion Constructors

        #region Properties

        public string IniFileName { get; set; }

        public string IniFilePath { get; set; }

        public Encoding Encoding { get; set; }

        public int BufferSize { get; set; }

        public int SectionCount => sections.Count;

        public ACBrIniSection this[string section]
        {
            get
            {
                var ret = sections.SingleOrDefault(x => x.Name == section);
                if (ret != null) return ret;

                ret = new ACBrIniSection(this, section);
                sections.Add(ret);

                return ret;
            }
        }

        #endregion Properties

        #region Methods

        /// <summary>
        /// Retorna true se a seção existir no ini.
        /// </summary>
        /// <param name="section"></param>
        /// <returns></returns>
        public bool Contains(string section)
        {
            return sections.Any(x => x.Name == section);
        }

        public bool Contains(ACBrIniSection section)
        {
            return sections.Contains(section);
        }

        public ACBrIniSection AddNew(string section)
        {
            var ret = new ACBrIniSection(this, section);
            sections.Add(ret);
            return ret;
        }

        public void Add(ACBrIniSection section)
        {
            sections.Add(section);
        }

        public void Remove(string section)
        {
            var ret = sections.Single(x => x.Name == section);
            sections.Remove(ret);
        }

        public void Remove(ACBrIniSection section)
        {
            sections.Remove(section);
        }

        public TType Read<TType>(string section, string propertie, TType defaultValue = default(TType), IFormatProvider format = null)
        {
            if (string.IsNullOrEmpty(propertie) || string.IsNullOrWhiteSpace(propertie)) return defaultValue;
            if (string.IsNullOrEmpty(section) || string.IsNullOrWhiteSpace(section)) return defaultValue;

            var iniSection = this[section];
            return iniSection.GetValue(propertie, defaultValue, format);
        }

        public void Write(string section, string propertie, object value)
        {
            if (string.IsNullOrEmpty(propertie) || string.IsNullOrWhiteSpace(propertie)) return;
            if (string.IsNullOrEmpty(section) || string.IsNullOrWhiteSpace(section)) return;

            var iniSection = this[section];
            iniSection.Add(propertie, string.Format(CultureInfo.InvariantCulture, "{0}", value));
        }

        public void Save()
        {
            var file = Path.Combine(IniFilePath, IniFileName);

            using (var writer = new StreamWriter(file, false, Encoding, 1024))
            {
                foreach (var section in sections)
                {
                    writer.WriteLine($"[{section.Name}]");

                    foreach (var iniData in section)
                    {
                        writer.WriteLine($"{iniData.Key}={iniData.Value}");
                    }

                    writer.WriteLine("");
                }

                writer.Flush();
            }
        }

        public static ACBrIniFile Load(string file, Encoding encoding = null)
        {
            if (!File.Exists(file)) throw new FileNotFoundException();

            var path = Path.GetDirectoryName(file);
            var iniFileName = Path.GetFileName(file);

            var ret = ParseIni(File.ReadAllText(file, encoding), encoding);

            ret.IniFileName = iniFileName;
            ret.IniFilePath = path;
            return ret;
        }

        public static ACBrIniFile Load(Stream stream, Encoding encoding = null)
        {
            if (stream == null) throw new ArgumentNullException(nameof(stream));

            encoding = encoding ?? Encoding.GetEncoding("ISO-8859-1");
            var iniFile = new ACBrIniFile { Encoding = encoding };

            using (var reader = new StreamReader(stream, iniFile.Encoding))
                return ParseIni(reader.ReadToEnd(), encoding);
        }

        public static ACBrIniFile ParseIni(string iniData, Encoding encoding = null)
        {
            if (string.IsNullOrEmpty(iniData) || string.IsNullOrWhiteSpace(iniData)) throw new ArgumentNullException(nameof(iniData));

            encoding = encoding ?? Encoding.GetEncoding("ISO-8859-1");
            var iniFile = new ACBrIniFile { Encoding = encoding };

            using (var reader = new StringReader(iniData))
            {
                string line;
                var section = string.Empty;
                while ((line = reader.ReadLine()) != null)
                {
                    line = line.Trim();

                    if (string.IsNullOrEmpty(line) || string.IsNullOrWhiteSpace(line)) continue;
                    if (line.StartsWith(";")) continue;

                    if (line.StartsWith("["))
                    {
                        section = line.Substring(1, line.Length - 2);
                        iniFile.sections.Add(new ACBrIniSection(iniFile, section));
                    }
                    else
                    {
                        if (string.IsNullOrEmpty(section) || string.IsNullOrWhiteSpace(section)) continue;

                        var iniSection = iniFile[section];
                        var properties = line.Split('=');
                        iniSection.Add(properties[0], properties[1]);
                    }
                }
            }

            return iniFile;
        }

        #endregion Methods

        #region IEnumerable

        /// <summary>
        /// Returns an enumerator that iterates through the collection.
        /// </summary>
        /// <returns>A <see cref="T:System.Collections.Generic.IEnumerator`1" /> that can be used to iterate through the collection.</returns>

        public IEnumerator<ACBrIniSection> GetEnumerator()
        {
            return sections.GetEnumerator();
        }

        /// <summary>
        /// Returns an enumerator that iterates through a collection.
        /// </summary>
        /// <returns>An <see cref="T:System.Collections.IEnumerator" /> object that can be used to iterate through the collection.</returns>
        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        #endregion IEnumerable
    }
}