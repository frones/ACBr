using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
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

        public ACBrIniSection this[int idx] => sections[idx];

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
            return Contains(section.Name);
        }

        public ACBrIniSection AddNew(string section)
        {
            if (Contains(section)) throw new ArgumentException("Sessão já existe no arquivo.");

            var ret = new ACBrIniSection(this, section);
            sections.Add(ret);
            return ret;
        }

        public void Add(ACBrIniSection section)
        {
            if (Contains(section.Name)) throw new ArgumentException("Sessão já existe no arquivo.");

            section.Parent = this;
            sections.Add(section);
        }

        public void Remove(string section)
        {
            var ret = sections.SingleOrDefault(x => x.Name == section);
            Remove(ret);
        }

        public void Remove(ACBrIniSection section)
        {
            if (section == null) return;
            Remove(section.Name);
            sections.Remove(section);
        }

        public TType Read<TType>(string section, string propertie, TType defaultValue = default)
        {
            if (string.IsNullOrEmpty(propertie) || string.IsNullOrWhiteSpace(propertie)) return defaultValue;
            if (string.IsNullOrEmpty(section) || string.IsNullOrWhiteSpace(section)) return defaultValue;

            var iniSection = this[section];
            return iniSection.GetValue(propertie, defaultValue);
        }

        public void Write(string section, string propertie, object value)
        {
            if (string.IsNullOrEmpty(propertie) || string.IsNullOrWhiteSpace(propertie)) return;
            if (string.IsNullOrEmpty(section) || string.IsNullOrWhiteSpace(section)) return;
            if (value == null) return;

            var iniSection = this[section];
            iniSection.SetValue(propertie, value);
        }

        public void Save()
        {
            if (string.IsNullOrEmpty(IniFilePath)) throw new ArgumentNullException();
            if (string.IsNullOrEmpty(IniFileName)) throw new ArgumentNullException();

            var file = Path.Combine(IniFilePath, IniFileName);

            using (var writer = new StreamWriter(file, false, Encoding, BufferSize))
                Save(writer);
        }

        public void Save(string file)
        {
            using (var writer = new StreamWriter(file, false, Encoding, BufferSize))
                Save(writer);
        }

        public void Save(Stream stream)
        {
            using (var writer = new StreamWriter(stream))
                Save(writer);
        }

        private void Save(TextWriter stream)
        {
            foreach (var section in sections)
                section.Save(stream);

            stream.Flush();
        }

        public static ACBrIniFile Parse(string iniData, Encoding encoding = null)
        {
            if (string.IsNullOrEmpty(iniData) || string.IsNullOrWhiteSpace(iniData)) throw new ArgumentNullException(nameof(iniData));

            encoding = encoding ?? Encoding.UTF8;
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
                        var idx = line.IndexOf('=', 0);
                        if (idx < 0) continue;

                        var key = line.Substring(0, idx).ToLower();
                        var value = idx >= line.Length - 1 ? "" : line.Substring(idx + 1);
                        iniSection.Add(key, value);
                    }
                }
            }

            return iniFile;
        }

        public static ACBrIniFile Load(string file, Encoding encoding = null)
        {
            if (!File.Exists(file)) throw new FileNotFoundException();

            using (var reader = new FileStream(file, FileMode.Open))
                return Load(reader, encoding);
        }

        public static ACBrIniFile Load(Stream stream, Encoding encoding = null)
        {
            if (stream == null) throw new ArgumentNullException(nameof(stream));

            encoding = encoding ?? Encoding.UTF8;
            var iniFile = new ACBrIniFile { Encoding = encoding };

            using (var reader = new StreamReader(stream, encoding))
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
                        var idx = line.IndexOf('=', 0);
                        if (idx < 0) continue;

                        var key = line.Substring(0, idx);
                        var value = idx >= line.Length - 1 ? "" : line.Substring(idx + 1);
                        iniSection.Add(key, value);
                    }
                }
            }

            return iniFile;
        }

        public override string ToString()
        {
            var builder = new StringBuilder();

            using (var writer = new StringWriter(builder))
                Save(writer);

            return builder.ToString();
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