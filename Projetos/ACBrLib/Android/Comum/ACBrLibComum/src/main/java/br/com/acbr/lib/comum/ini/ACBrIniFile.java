package br.com.acbr.lib.comum.ini;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class ACBrIniFile implements Iterable<ACBrIniSection> {

    private List<ACBrIniSection> sections;
    private String iniFilePath;
    private String iniFileName;
    private Charset encoding;
    private int bufferSize;

    public ACBrIniFile() {
        this("", "", StandardCharsets.ISO_8859_1, 1024);
    }

    public ACBrIniFile(String iniFilePath, String iniFileName) {
        this(iniFilePath, iniFileName, StandardCharsets.ISO_8859_1, 1024);
    }

    public ACBrIniFile(String iniFilePath, String iniFileName, Charset encoding, int bufferSize) {
        this.sections = new ArrayList<>();
        this.encoding = encoding;
        this.bufferSize = bufferSize;
        this.iniFilePath = iniFilePath;
        this.iniFileName = iniFileName;
    }

    public String getIniFileName() {
        return iniFileName;
    }

    public void setIniFileName(String iniFileName) {
        this.iniFileName = iniFileName;
    }

    public String getIniFilePath() {
        return iniFilePath;
    }

    public void setIniFilePath(String iniFilePath) {
        this.iniFilePath = iniFilePath;
    }

    public Charset getEncoding() {
        return encoding;
    }

    public void setEncoding(Charset encoding) {
        this.encoding = encoding;
    }

    public int getBufferSize() {
        return bufferSize;
    }

    public void setBufferSize(int bufferSize) {
        this.bufferSize = bufferSize;
    }

    public int getSectionCount() {
        return sections.size();
    }

    public ACBrIniSection getSection(int idx) {
        return sections.get(idx);
    }

    public ACBrIniSection getSection(String section) {
        for (ACBrIniSection sec : sections) {
            if (sec.getName().equals(section)) {
                return sec;
            }
        }
        ACBrIniSection newSection = new ACBrIniSection(this, section);
        sections.add(newSection);
        return newSection;
    }

    public boolean contains(String section){
        return sections.stream().anyMatch(s -> s.getName().equals(section));
    }

    public boolean contains(ACBrIniSection section) {
        return contains(section.getName());
    }

    public ACBrIniSection addNew(String section) {
        if (contains(section)) {
            throw new IllegalArgumentException("Sessão já existe no arquivo.");
        }
        ACBrIniSection newSection = new ACBrIniSection(this, section);
        sections.add(newSection);
        return newSection;
    }

    public void add(ACBrIniSection section) {
        if (contains(section.getName())) {
            throw new IllegalArgumentException("Sessão já existe no arquivo.");
        }
        section.setParent(this);
        sections.add(section);
    }

    public void remove(String section) {
        sections.removeIf(s -> s.getName().equals(section));
    }

    public void remove(ACBrIniSection section) {
        sections.remove(section);
    }

    public <T> T read(String section, String property, T defaultValue) {
        if (property == null || property.trim().isEmpty() || section == null || section.trim().isEmpty()) {
            return defaultValue;
        }
        ACBrIniSection iniSection = getSection(section);
        return iniSection.getValue(property, defaultValue);
    }

    public void write(String section, String property, Object value) {
        if (property == null || property.trim().isEmpty() || section == null || section.trim().isEmpty() || value == null) {
            return;
        }
        ACBrIniSection iniSection = getSection(section);
        iniSection.setValue(property, value);
    }

    public void save() throws IOException {
        if (iniFilePath == null || iniFileName == null) {
            throw new IllegalArgumentException("O caminho e o nome do arquivo devem ser especificados.");
        }
        File file = new File(iniFilePath, iniFileName);
        try (Writer writer = new OutputStreamWriter(new FileOutputStream(file), encoding)) {
            save(writer);
        }
    }

    public void save(String file) throws IOException {
        try (Writer writer = new OutputStreamWriter(new FileOutputStream(file), encoding)) {
            save(writer);
        }
    }

    public void save(OutputStream stream) throws IOException {
        try (Writer writer = new OutputStreamWriter(stream, encoding)) {
            save(writer);
        }
    }

    private void save(Writer writer) throws IOException {
        for (ACBrIniSection section : sections) {
            section.save(writer);
        }
        writer.flush();
    }

    public static ACBrIniFile parse(String iniData, Charset encoding) {
        if (iniData == null || iniData.trim().isEmpty()) {
            throw new IllegalArgumentException("Invalid ini data.");
        }
        ACBrIniFile iniFile = new ACBrIniFile();
        iniFile.encoding = encoding != null ? encoding : StandardCharsets.UTF_8;
        try (BufferedReader reader = new BufferedReader(new StringReader(iniData))) {
            String line;
            String currentSection = null;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty() || line.startsWith(";")) continue;
                if (line.startsWith("[")) {
                    currentSection = line.substring(1, line.length() - 1);
                    iniFile.sections.add(new ACBrIniSection(iniFile, currentSection));
                } else if (currentSection != null) {
                    String[] parts = line.split("=", 2);
                    String key = parts[0].trim();
                    String value = parts.length > 1 ? parts[1].trim() : "";
                    iniFile.getSection(currentSection).put(key, value);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return iniFile;
    }

    public static ACBrIniFile load(String file, Charset encoding) throws IOException {
        File f = new File(file);
        if (!f.exists()) throw new FileNotFoundException();
        try (InputStream stream = new FileInputStream(f)) {
            return load(stream, encoding);
        }
    }

    public static ACBrIniFile load(InputStream stream, Charset encoding) throws IOException {
        ACBrIniFile iniFile = new ACBrIniFile();
        iniFile.encoding = encoding != null ? encoding : StandardCharsets.UTF_8;
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(stream, iniFile.encoding))) {
            String line;
            String currentSection = null;
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty() || line.startsWith(";")) continue;
                if (line.startsWith("[")) {
                    currentSection = line.substring(1, line.length() - 1);
                    iniFile.sections.add(new ACBrIniSection(iniFile, currentSection));
                } else if (currentSection != null) {
                    String[] parts = line.split("=", 2);
                    String key = parts[0].trim();
                    String value = parts.length > 1 ? parts[1].trim() : "";
                    iniFile.getSection(currentSection).put(key, value);
                }
            }
        }
        return iniFile;
    }

    public String toString() {
        StringWriter writer = new StringWriter();
        try {
            save(writer);
        } catch (IOException e) {
            e.printStackTrace();
        }
        return writer.toString();
    }

    @Override
    public Iterator<ACBrIniSection> iterator(){
        return sections.iterator();
    }

}
