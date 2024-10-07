package com.acbr.nfe.acbrlibnfe.demo;

import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Spinner;

import androidx.fragment.app.Fragment;

import br.com.acbr.lib.comum.dfe.SSLCryptLib;
import br.com.acbr.lib.comum.dfe.SSLHttpLib;
import br.com.acbr.lib.comum.dfe.SSLXmlSignLib;
import br.com.acbr.lib.nfe.ACBrLibNFe;

public class ConfiguracoesCertificadosFragment extends Fragment {

    private ACBrLibNFe ACBrNFe;

    private NfeApplication application;
    private EditText txtCertPath;
    private EditText txtDadosPFX;
    private EditText txtCertPassword;
    private EditText txtCertNumero;
    private EditText txtRespostaCertificado;
    private Spinner cmbCrypt;
    private Spinner cmbHttp;
    private Spinner cmbXmlSign;
    private Button btnSalvarConfiguracoesCertificados;
    private Button btnCarregarConfiguracoesCertificados;
    private Button btnObterCertificados;

    SSLCryptLib[] sslCryptLibs = SSLCryptLib.values();
    SSLHttpLib[] sslHttpLibs = SSLHttpLib.values();
    SSLXmlSignLib[] sslXmlSignLibs = SSLXmlSignLib.values();

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_certificados, container, false);

        ACBrNFe = ACBrLibHelper.getInstance("");

        cmbCrypt = view.findViewById(R.id.cmbCrypt);
        cmbHttp = view.findViewById(R.id.cmbHttp);
        cmbXmlSign = view.findViewById(R.id.cmbXmlSign);
        txtCertPath = view.findViewById(R.id.txtCertPath);
        txtDadosPFX = view.findViewById(R.id.txtDadosPFX);
        txtCertPassword = view.findViewById(R.id.txtCertPassword);
        txtCertNumero = view.findViewById(R.id.txtCertNumero);
        txtRespostaCertificado = view.findViewById(R.id.txtRespostaCertificado);
        btnObterCertificados = view.findViewById(R.id.btnObterCertificados);
        btnSalvarConfiguracoesCertificados = view.findViewById(R.id.btnSalvarConfiguracoesCertificados);
        btnCarregarConfiguracoesCertificados = view.findViewById(R.id.btnCarregarConfiguracoesCertificados);
        application = ((NfeApplication) this.getContext().getApplicationContext());
        txtCertPath.setText(application.getPfxPath());

        SpinnerUtils.preencherSpinner(getContext(), cmbCrypt, sslCryptLibs);
        SpinnerUtils.preencherSpinner(getContext(), cmbHttp, sslHttpLibs);
        SpinnerUtils.preencherSpinner(getContext(), cmbXmlSign, sslXmlSignLibs);

        btnSalvarConfiguracoesCertificados.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesCertificados();
            }
        });

        btnCarregarConfiguracoesCertificados.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesCertificados();
            }
        });

        btnObterCertificados.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                obterCertificados();
            }
        });

        txtCertPath.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                ((ConfiguracoesNFeActivity) getActivity()).getCertificate();
            }
        });

        carregarConfiguracoesCertificados();

        return view;
    }

    private void salvarConfiguracoesCertificados() {
        try {
            ACBrNFe.configGravarValor("DFe", "SSLCryptLib", Integer.toString(cmbCrypt.getSelectedItemPosition()));
            ACBrNFe.configGravarValor("DFe", "SSLHttpLib", Integer.toString(cmbHttp.getSelectedItemPosition()));
            ACBrNFe.configGravarValor("DFe", "SSLXmlSignLib", Integer.toString(cmbXmlSign.getSelectedItemPosition()));
            ACBrNFe.configGravarValor("DFe", "ArquivoPFX", application.getPfxPath());
            ACBrNFe.configGravarValor("DFe", "DadosPFX", txtDadosPFX.getText().toString());
            ACBrNFe.configGravarValor("DFe", "Senha", txtCertPassword.getText().toString());
            ACBrNFe.configGravarValor("DFe", "NumeroSerie", txtCertNumero.getText().toString());
            ACBrNFe.configGravar();
        } catch (Exception ex) {
            Log.i("Erro", " - Salvar Configurações Certificados: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesCertificados() {
        try {
            cmbCrypt.setSelection(Integer.valueOf(ACBrNFe.configLerValor("DFe", "SSLCryptLib")));
            cmbHttp.setSelection(Integer.valueOf(ACBrNFe.configLerValor("DFe", "SSLHttpLib")));
            cmbXmlSign.setSelection(Integer.valueOf(ACBrNFe.configLerValor("DFe", "SSLXmlSignLib")));
            txtCertPath.setText(ACBrNFe.configLerValor("DFe", "ArquivoPFX"));
            txtDadosPFX.setText(ACBrNFe.configLerValor("DFe", "DadosPFX"));
            txtCertPassword.setText(ACBrNFe.configLerValor("DFe", "Senha"));
            txtCertNumero.setText(ACBrNFe.configLerValor("DFe", "NumeroSerie"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações Certificados: " + ex.getMessage());
        }
    }

    private void obterCertificados() {
        txtRespostaCertificado.setText("");
        String result = "";
        try {
            result = ACBrNFe.ObterCertificados();
        } catch (Exception e) {
            Log.e("Erro Obter Certificado", e.getMessage());
            result = e.getMessage();
        } finally {
            txtRespostaCertificado.setText(result);
        }
    }
}