package com.acbr.nfe.acbrlibnfe.demo;

import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.Spinner;
import android.widget.TextView;

import androidx.fragment.app.Fragment;

import java.util.Arrays;

import br.com.acbr.lib.nfe.ACBrLibNFe;
import br.com.acbr.lib.nfe.SSLType;

public class ConfiguracoesWebServicesFragment extends Fragment {
    private ACBrLibNFe ACBrNFe;

    private CheckBox ckbVisualizar;
    private CheckBox ckbSalvarSOAP;
    private CheckBox ckbAjustarAut;
    private TextView txtTimeOut;
    private TextView txtAguardar;
    private TextView txtTentativas;
    private TextView txtIntervalos;
    private TextView txtProxyServidor;
    private TextView txtProxyPorta;
    private TextView txtProxyUsuario;
    private TextView txtProxySenha;
    private Spinner cmbUfDestino;
    private Spinner cmbSSlType;
    private Spinner cmbAmbiente;
    private Button btnSalvarConfiguracoesWebServices;
    private Button btnCarregarConfiguracoesWebServices;

    private String[] ufs = {"Nenhum", "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"};
    private SSLType[] sslType = SSLType.values();
    private String[] ambiente = {"Produção", "Homologação"};

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_web_services, container, false);

        ACBrNFe = ACBrLibHelper.getInstance("");

        cmbUfDestino = view.findViewById(R.id.cmbUfDestino);
        cmbSSlType = view.findViewById(R.id.cmbSSlType);
        txtTimeOut = view.findViewById(R.id.txtTimeOut);
        cmbAmbiente = view.findViewById(R.id.cmbAmbiente);
        ckbVisualizar = view.findViewById(R.id.ckbVisualizar);
        ckbSalvarSOAP = view.findViewById(R.id.ckbSalvarSOAP);
        ckbAjustarAut = view.findViewById(R.id.ckbAjustarAut);
        txtAguardar = view.findViewById(R.id.txtAguardar);
        txtTentativas = view.findViewById(R.id.txtTentativas);
        txtIntervalos = view.findViewById(R.id.txtIntervalos);
        txtProxyServidor = view.findViewById(R.id.txtProxyServidor);
        txtProxyPorta = view.findViewById(R.id.txtProxyPorta);
        txtProxyUsuario = view.findViewById(R.id.txtProxyUsuario);
        txtProxySenha = view.findViewById(R.id.txtProxySenha);

        btnSalvarConfiguracoesWebServices = view.findViewById(R.id.btnSalvarConfiguracoesWebServices);
        btnCarregarConfiguracoesWebServices = view.findViewById(R.id.btnCarregarConfiguracoesWebServices);

        SpinnerUtils.preencherSpinner(getContext(), cmbUfDestino, ufs);
        SpinnerUtils.preencherSpinner(getContext(), cmbSSlType, sslType);
        SpinnerUtils.preencherSpinner(getContext(), cmbAmbiente, ambiente);

        btnSalvarConfiguracoesWebServices.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesWebServices();
            }
        });

        btnCarregarConfiguracoesWebServices.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesWebServices();
            }
        });

        carregarConfiguracoesWebServices();

        return view;
    }

    private void salvarConfiguracoesWebServices() {
        try {
            ACBrNFe.configGravarValor("DFe", "UF", cmbUfDestino.getSelectedItem().toString());
            ACBrNFe.configGravarValor("NFe", "SSLType", Integer.toString(cmbSSlType.getSelectedItemPosition()));
            ACBrNFe.configGravarValor("NFe", "TimeOut", txtTimeOut.getText().toString());
            ACBrNFe.configGravarValor("NFe", "Ambiente", Integer.toString(cmbAmbiente.getSelectedItemPosition()));
            ACBrNFe.configGravarValor("NFe", "Visualizar", ckbVisualizar.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("NFe", "SalvarWS", ckbSalvarSOAP.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("NFe", "AjustaAguardaConsultaRet", ckbAjustarAut.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("NFe", "AguardarConsultaRet", txtAguardar.getText().toString());
            ACBrNFe.configGravarValor("NFe", "Tentativas", txtTentativas.getText().toString());
            ACBrNFe.configGravarValor("NFe", "IntervaloTentativas", txtIntervalos.getText().toString());
            ACBrNFe.configGravarValor("Proxy", "Servidor", txtProxyServidor.getText().toString());
            ACBrNFe.configGravarValor("Proxy", "Porta", txtProxyPorta.getText().toString());
            ACBrNFe.configGravarValor("Proxy", "Usuario", txtProxyUsuario.getText().toString());
            ACBrNFe.configGravarValor("Proxy", "Senha", txtProxySenha.getText().toString());
            ACBrNFe.configGravar();
        } catch (Exception ex) {
            Log.i("Erro", " - Salvar Configurações WebServices: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesWebServices() {
        try {
            cmbUfDestino.setSelection(Integer.valueOf(Arrays.asList(ufs).indexOf(ACBrNFe.configLerValor("DFe", "UF"))));
            cmbSSlType.setSelection(Integer.valueOf(ACBrNFe.configLerValor("NFe", "SSLType")));
            txtTimeOut.setText(ACBrNFe.configLerValor("NFe", "TimeOut"));
            cmbAmbiente.setSelection(Integer.valueOf(ACBrNFe.configLerValor("NFe", "Ambiente")));
            ckbVisualizar.setChecked("1".equals(ACBrNFe.configLerValor("NFe", "Visualizar")));
            ckbSalvarSOAP.setChecked("1".equals(ACBrNFe.configLerValor("NFe", "SalvarWS")));
            ckbAjustarAut.setChecked("1".equals(ACBrNFe.configLerValor("NFe", "AjustaAguardaConsultaRet")));
            txtAguardar.setText(ACBrNFe.configLerValor("NFe", "AguardarConsultaRet"));
            txtTentativas.setText(ACBrNFe.configLerValor("NFe", "Tentativas"));
            txtIntervalos.setText(ACBrNFe.configLerValor("NFe", "IntervaloTentativas"));
            txtProxyServidor.setText(ACBrNFe.configLerValor("Proxy", "Servidor"));
            txtProxyPorta.setText(ACBrNFe.configLerValor("Proxy", "Porta"));
            txtProxyUsuario.setText(ACBrNFe.configLerValor("Proxy", "Usuario"));
            txtProxySenha.setText(ACBrNFe.configLerValor("Proxy", "Senha"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações WebServices: " + ex.getMessage());
        }
    }
}

