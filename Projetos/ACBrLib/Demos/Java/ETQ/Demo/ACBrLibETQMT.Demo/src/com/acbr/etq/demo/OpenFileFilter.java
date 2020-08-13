/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr.etq.demo;

/**
 *
 * @author rften
 */
import java.io.File;
import javax.swing.filechooser.*;

public class OpenFileFilter extends FileFilter {

    String description = "";
    String fileExt = "";

    public OpenFileFilter(String extension) {
        fileExt = extension;
    }

    public OpenFileFilter(String extension, String typeDescription) {
        fileExt = extension;
        this.description = typeDescription;
    }

    @Override
    public boolean accept(File f) {
        if (f.isDirectory())
            return true;
        
        String[] exts = fileExt.split("\\|");
        for(String ext : exts){
            if((f.getName().toLowerCase().endsWith(ext)))
                return true;
        }
        
        return false;
    }

    @Override
    public String getDescription() {
        return description;
    }
}
