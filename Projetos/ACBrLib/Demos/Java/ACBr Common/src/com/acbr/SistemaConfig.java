/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr;

import java.util.Date;

/**
 *
 * @author RFTD
 * @param <TLib>
 */
public final class SistemaConfig<TLib extends ACBrLibBase> extends ACBrLibConfigBase<TLib>{
    
    public SistemaConfig(TLib acbrlib) {
        super(acbrlib, ACBrSessao.Sistema);
    }
    
    public String getNome() throws Exception{
       return getProperty("Nome");
    }
    
    public void setNome(String value) throws Exception {
        this.setProperty("Nome", value);
    }
    
    public String getVersao() throws Exception{
       return getProperty("Versao");
    }
    
    public void setVersao(String value) throws Exception {
        this.setProperty("Versao", value);
    }
    
    public Date getData() throws Exception{
       return getDateProperty("Data");
    }
    
    public void setData(Date value) throws Exception {
        this.setDateProperty("Data", value);
    }
    
    public String getDescricao() throws Exception{
       return getProperty("Descricao");
    }
    
    public void setDescricao(String value) throws Exception {
        this.setProperty("Descricao", value);
    }
    
}
