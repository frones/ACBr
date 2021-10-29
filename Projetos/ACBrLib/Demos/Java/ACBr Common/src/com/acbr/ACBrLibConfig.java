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
public abstract class ACBrLibConfig<TLib extends ACBrLibBase> extends ACBrLibConfigBase<TLib> {
    
    private final PrincipalConfig<TLib> principal;
    private final SistemaConfig<TLib> sistema;
    
    public ACBrLibConfig(TLib acbrlib, ACBrSessao sessao) {
        super(acbrlib, sessao);
        
        principal = new PrincipalConfig<>(acbrlib);
        sistema = new SistemaConfig<>(acbrlib);
    }
    
    public PrincipalConfig<TLib> getPrincipal(){
        return principal;
    }
    
    public SistemaConfig<TLib> getSistema(){
        return sistema;
    }
    
}
