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
public final class PrincipalConfig<TLib extends ACBrLibBase> extends ACBrLibConfigBase<TLib>{
    
    public PrincipalConfig(TLib acbrlib) {
        super(acbrlib, ACBrSessao.Principal);
    }
    
    public TipoResposta getTipoResposta() throws Exception{
       return TipoResposta.valueOf(getIntProperty("TipoResposta"));
    }
    
    public void setTipoResposta(TipoResposta value) throws Exception {
        this.setIntProperty("TipoResposta", value.asInt());
    }
    
    public CodResposta getCodificacaoResposta() throws Exception{
       return CodResposta.valueOf(getIntProperty("CodificacaoResposta"));
    }
    
    public void setCodificacaoResposta(CodResposta value) throws Exception {
        this.setIntProperty("CodificacaoResposta", value.asInt());
    }
    
    public NivelLog getLogNivel() throws Exception{
       return NivelLog.valueOf(getIntProperty("LogNivel"));
    }
    
    public void setLogNivel(NivelLog value) throws Exception {
        this.setIntProperty("LogNivel", value.asInt());
    }
    
    public String getLogPath() throws Exception{
       return getProperty("LogPath");
    }
    
    public void setLogPath(String value) throws Exception {
        this.setProperty("LogPath", value);
    }    
}
