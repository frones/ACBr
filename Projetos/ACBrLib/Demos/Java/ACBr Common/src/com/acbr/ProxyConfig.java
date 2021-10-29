/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.acbr;

/**
 *
 * @author rften
 * @param <TLib>
 */
public final class ProxyConfig<TLib extends ACBrLibBase> extends ACBrLibConfigBase<TLib>{
    
    public ProxyConfig(TLib acbrlib) {
        super(acbrlib, ACBrSessao.Proxy);
    }
    
    public String getServidor() throws Exception{
       return getProperty("Servidor");
    }
    
    public void setServidor(String value) throws Exception {
        this.setProperty("Servidor", value);
    }  
    
    public String getPorta() throws Exception{
       return getProperty("Porta");
    }
    
    public void setPorta(String value) throws Exception {
        this.setProperty("Porta", value);
    } 
    
    public String getUsuario() throws Exception{
       return getProperty("Usuario");
    }
    
    public void setUsuario(String value) throws Exception {
        this.setProperty("Usuario", value);
    } 
    
    public String getSenha() throws Exception{
       return getProperty("Senha");
    }
    
    public void setSenha(String value) throws Exception {
        this.setProperty("Senha", value);
    } 
}
