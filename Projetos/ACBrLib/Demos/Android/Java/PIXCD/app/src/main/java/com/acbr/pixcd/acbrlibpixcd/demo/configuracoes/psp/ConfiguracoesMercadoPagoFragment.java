package com.acbr.pixcd.acbrlibpixcd.demo.configuracoes.psp;

import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ACBrLibHelper;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ConfiguracoesMercadoPagoFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtChavePIXMercadoPago;
    private EditText txtAccessTokenMercadoPago;
    private EditText txtScopesMercadoPago;
    private Button btnSalvarConfiguracoesMercadoPago;
    private Button btnCarregarConfiguracoesMercadoPago;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_mercadopago, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtChavePIXMercadoPago = view.findViewById(R.id.txtChavePIXMercadoPago);
        txtAccessTokenMercadoPago = view.findViewById(R.id.txtAccessTokenMercadoPago);
        txtScopesMercadoPago = view.findViewById(R.id.txtScopesMercadoPago);
        btnSalvarConfiguracoesMercadoPago = view.findViewById(R.id.btnSalvarConfiguracoesMercadoPago);
        btnCarregarConfiguracoesMercadoPago = view.findViewById(R.id.btnCarregarConfiguracoesMercadoPago);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnSalvarConfiguracoesMercadoPago.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesMercadoPago();
            }
        });

        btnCarregarConfiguracoesMercadoPago.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesMercadoPago();
            }
        });

        carregarConfiguracoesMercadoPago();

        return view;
    }

    private void salvarConfiguracoesMercadoPago(){
        try{
            ACBrPIXCD.configGravarValor("MercadoPago", "ChavePIX", txtChavePIXMercadoPago.getText().toString());
            ACBrPIXCD.configGravarValor("MercadoPago", "AccessToken", txtAccessTokenMercadoPago.getText().toString());
            ACBrPIXCD.configGravarValor("MercadoPago", "Scopes", txtScopesMercadoPago.getText().toString());
            ACBrPIXCD.configGravar();
        }catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações Mercado Pago: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesMercadoPago(){
        try{
            txtChavePIXMercadoPago.setText(ACBrPIXCD.configLerValor("MercadoPago", "ChavePIX"));
            txtAccessTokenMercadoPago.setText(ACBrPIXCD.configLerValor("MercadoPago", "AccessToken"));
            txtScopesMercadoPago.setText(ACBrPIXCD.configLerValor("MercadoPago", "Scopes"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações Mercado Pago: " + ex.getMessage());
        }
    }
}