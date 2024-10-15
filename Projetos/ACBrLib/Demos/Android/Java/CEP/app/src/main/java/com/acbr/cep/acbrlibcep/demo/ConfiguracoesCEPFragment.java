package com.acbr.cep.acbrlibcep.demo;

import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.Spinner;
import android.widget.TextView;

import br.com.acbr.lib.cep.ACBrLibCep;
import br.com.acbr.lib.cep.WebServices;

public class ConfiguracoesCEPFragment extends Fragment {
    private ACBrLibCep ACBrCEP;

    private Spinner cmbWebService;
    private CheckBox ckbPesquisarIBGE;
    private TextView txtUsuarioWebService;
    private TextView txtSenhaWebService;
    private TextView txtChaveWebService;
    private TextView txtProxyServidor;
    private TextView txtProxyPorta;
    private TextView txtProxyUsuario;
    private TextView txtProxySenha;
    private Button btnSalvarConfiguracoesCEP;
    private Button btnCarregarConfiguracoesCEP;

    private WebServices[] webservices = WebServices.values();

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_cep, container, false);

        ACBrCEP = ACBrLibHelper.getInstance("");

        cmbWebService = view.findViewById(R.id.cmbWebService);
        ckbPesquisarIBGE = view.findViewById(R.id.ckbPesquisarIBGE);
        txtUsuarioWebService = view.findViewById(R.id.txtUsuarioWebService);
        txtSenhaWebService = view.findViewById(R.id.txtSenhaWebService);
        txtChaveWebService = view.findViewById(R.id.txtChaveWebService);
        txtProxyServidor = view.findViewById(R.id.txtProxyServidor);
        txtProxyPorta = view.findViewById(R.id.txtProxyPorta);
        txtProxyUsuario = view.findViewById(R.id.txtProxyUsuario);
        txtProxySenha = view.findViewById(R.id.txtProxySenha);

        btnSalvarConfiguracoesCEP = view.findViewById(R.id.btnSalvarConfiguracoesCEP);
        btnCarregarConfiguracoesCEP = view.findViewById(R.id.btnCarregarConfiguracoesCEP);

        SpinnerUtils.preencherSpinner(getContext(), cmbWebService, webservices);

        btnSalvarConfiguracoesCEP.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesCEP();
            }
        });

        btnCarregarConfiguracoesCEP.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesCEP();
            }
        });

        carregarConfiguracoesCEP();

        return view;
    }

    private void salvarConfiguracoesCEP() {
        try{
            ACBrCEP.configGravarValor("CEP", "WebService", Integer.toString(cmbWebService.getSelectedItemPosition()));
            ACBrCEP.configGravarValor("CEP", "PesquisarIBGE", ckbPesquisarIBGE.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrCEP.configGravarValor("CEP", "Usuario", txtUsuarioWebService.getText().toString());
            ACBrCEP.configGravarValor("CEP", "Senha", txtSenhaWebService.getText().toString());
            ACBrCEP.configGravarValor("CEP", "ChaveAcesso", txtChaveWebService.getText().toString());
            ACBrCEP.configGravarValor("Proxy", "Servidor", txtProxyServidor.getText().toString());
            ACBrCEP.configGravarValor("Proxy", "Porta", txtProxyPorta.getText().toString());
            ACBrCEP.configGravarValor("Proxy", "Usuario", txtProxyUsuario.getText().toString());
            ACBrCEP.configGravarValor("Proxy", "Senha", txtProxySenha.getText().toString());
            ACBrCEP.configGravar();
        } catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações CEP: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesCEP() {
        try{
            cmbWebService.setSelection(Integer.valueOf(ACBrCEP.configLerValor("CEP", "WebService")));
            ckbPesquisarIBGE.setChecked("1".equals(ACBrCEP.configLerValor("CEP", "PesquisarIBGE")));
            txtUsuarioWebService.setText(ACBrCEP.configLerValor("CEP", "Usuario"));
            txtSenhaWebService.setText(ACBrCEP.configLerValor("CEP", "Senha"));
            txtChaveWebService.setText(ACBrCEP.configLerValor("CEP", "ChaveAcesso"));
            txtProxyServidor.setText(ACBrCEP.configLerValor("Proxy", "Servidor"));
            txtProxyPorta.setText(ACBrCEP.configLerValor("Proxy", "Porta"));
            txtProxyUsuario.setText(ACBrCEP.configLerValor("Proxy", "Usuario"));
            txtProxySenha.setText(ACBrCEP.configLerValor("Proxy", "Senha"));
        } catch (Exception ex){
            Log.i("Erro", " - Carregar Configurações CEP: " + ex.getMessage());
        }
    }
}