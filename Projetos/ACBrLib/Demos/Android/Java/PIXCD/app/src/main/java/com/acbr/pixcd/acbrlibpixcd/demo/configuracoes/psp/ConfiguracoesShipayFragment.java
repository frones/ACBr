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

public class ConfiguracoesShipayFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtClientIDShipay;
    private EditText txtSecretKeyShipay;
    private EditText txtAccessKeyShipay;
    private EditText txtScopesShipay;
    private Button btnSalvarConfiguracoesShipay;
    private Button btnCarregarConfiguracoesShipay;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_shipay, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtClientIDShipay = view.findViewById(R.id.txtClientIDShipay);
        txtSecretKeyShipay = view.findViewById(R.id.txtSecretKeyShipay);
        txtAccessKeyShipay = view.findViewById(R.id.txtAccessKeyShipay);
        txtScopesShipay = view.findViewById(R.id.txtScopesShipay);
        btnSalvarConfiguracoesShipay = view.findViewById(R.id.btnSalvarConfiguracoesShipay);
        btnCarregarConfiguracoesShipay = view.findViewById(R.id.btnCarregarConfiguracoesShipay);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnSalvarConfiguracoesShipay.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesShipay();
            }
        });

        btnCarregarConfiguracoesShipay.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesShipay();
            }
        });

        carregarConfiguracoesShipay();

        return view;
    }

    private void salvarConfiguracoesShipay() {
        try{
            ACBrPIXCD.configGravarValor("Shipay", "ClientID", txtClientIDShipay.getText().toString());
            ACBrPIXCD.configGravarValor("Shipay", "SecretKey", txtSecretKeyShipay.getText().toString());
            ACBrPIXCD.configGravarValor("Shipay", "AccessKey", txtAccessKeyShipay.getText().toString());
            ACBrPIXCD.configGravarValor("Shipay", "Scopes", txtScopesShipay.getText().toString());
            ACBrPIXCD.configGravar();
        }catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações Shipay: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesShipay() {
        try{
            txtClientIDShipay.setText(ACBrPIXCD.configLerValor("Shipay", "ClientID"));
            txtSecretKeyShipay.setText(ACBrPIXCD.configLerValor("Shipay", "SecretKey"));
            txtAccessKeyShipay.setText(ACBrPIXCD.configLerValor("Shipay", "AccessKey"));
            txtScopesShipay.setText(ACBrPIXCD.configLerValor("Shipay", "Scopes"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações Shipay: " + ex.getMessage());
        }
    }
}