/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr;

import java.text.SimpleDateFormat;
import java.util.Date;

/**
 *
 * @author rften
 * @param <TLib>
 */
public abstract class ACBrLibConfigBase<TLib extends ACBrLibBase> {
    protected TLib parent;
    protected ACBrSessao sessaoConfig;
    protected String subName;
    
    protected ACBrLibConfigBase(TLib acbrlib, ACBrSessao sessao){
        parent = acbrlib;
        sessaoConfig = sessao;
    }
       
    private String getPropertyName(String propertyName){
        if ("".equals(subName))
            return propertyName;
        else
            return subName + "." + propertyName;
    }
    
    protected String getProperty(String propertyName) throws Exception{
        return parent.configLerValor(sessaoConfig,  getPropertyName(propertyName));         
    }
    
    protected void setProperty(String propertyName, String value) throws Exception{
        parent.configGravarValor(sessaoConfig,  getPropertyName(propertyName), value);        
    }
    
    protected int getIntProperty(String propertyName) throws Exception{
        String value = getProperty(propertyName);
        return Integer.parseInt(value);
    }
    
    protected void setIntProperty(String propertyName, int value) throws Exception{
        String svalue = String.valueOf(value);
        setProperty(propertyName, svalue);
    }
    
    protected boolean getBoolProperty(String propertyName) throws Exception{
        String value = getProperty(propertyName);
        return Boolean.parseBoolean(value);
    }
    
    protected void setBoolProperty(String propertyName, boolean value) throws Exception{
        String svalue = value ? "1" : "0";
        setProperty(propertyName, svalue);
    }
    
    protected Date getDateProperty(String propertyName) throws Exception{
        String value = getProperty(propertyName);
        SimpleDateFormat formato = new SimpleDateFormat("dd/MM/yyyy"); 
        return formato.parse(value);
    }
    
    protected void setDateProperty(String propertyName, Date value) throws Exception{
        SimpleDateFormat formato = new SimpleDateFormat("dd/MM/yyyy"); 
        String svalue = formato.format(value);
        setProperty(propertyName, svalue);
    }
}
