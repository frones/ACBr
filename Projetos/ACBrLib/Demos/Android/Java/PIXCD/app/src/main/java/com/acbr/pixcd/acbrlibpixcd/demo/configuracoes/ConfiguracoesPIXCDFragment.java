package com.acbr.pixcd.acbrlibpixcd.demo.configuracoes;

import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Spinner;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ACBrLibHelper;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.SpinnerUtils;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ConfiguracoesPIXCDFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtNomeRecebedor;
    private EditText txtCidadeRecebedor;
    private EditText txtCEPRecebedor;
    private EditText txtUFRecebedor;
    private Spinner cmbPSP;
    private Spinner cmbTipoChave;
    private Spinner cmbAmbiente;
    private EditText txtTimeoutPSP;
    private EditText txtArquivoLogPSP;
    private Spinner cmbNivelLogPSP;
    private EditText txtProxyServidor;
    private EditText txtProxyPorta;
    private EditText txtProxyUsuario;
    private EditText txtProxySenha;
    private Button btnSalvarConfiguracoesPIXCD;
    private Button btnCarregarConfiguracoesPIXCD;

    private String[] ambiente = {"Teste", "Produção", "Pré-Produção"};
    private String[] nivelLogPSP = {"Nenhum", "Baixo", "Normal", "Alto", "Muito Alto"};
    private String[] tipoChave = {"Nenhum", "Email", "CPF", "CNPJ", "Celular", "Aleatoria"};
    private String[] psp = {"Bradesco", "Itaú", "Banco do Brasil", "Santander", "Shipay",
                            "Sicredi", "Sicoob", "PagSeguro", "GerenciaNet", "PixPDV", "Inter", "Ailos",
                            "Matera", "Cielo", "Mercado Pago", "Gate2All", "Banrisul", "C6Bank"};

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_pixcd, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtNomeRecebedor = view.findViewById(R.id.txtNomeRecebedor);
        txtCidadeRecebedor = view.findViewById(R.id.txtCidadeRecebedor);
        txtCEPRecebedor = view.findViewById(R.id.txtCEPRecebedor);
        txtUFRecebedor = view.findViewById(R.id.txtUFRecebedor);
        cmbPSP = view.findViewById(R.id.cmbPSP);
        cmbTipoChave = view.findViewById(R.id.cmbTipoChave);
        cmbAmbiente = view.findViewById(R.id.cmbAmbiente);
        txtTimeoutPSP = view.findViewById(R.id.txtTimeoutPSP);
        txtArquivoLogPSP = view.findViewById(R.id.txtArquivoLogPSP);
        cmbNivelLogPSP = view.findViewById(R.id.cmbNivelLogPSP);
        txtProxyServidor = view.findViewById(R.id.txtProxyServidor);
        txtProxyPorta = view.findViewById(R.id.txtProxyPorta);
        txtProxyUsuario = view.findViewById(R.id.txtProxyUsuario);
        txtProxySenha = view.findViewById(R.id.txtProxySenha);
        btnSalvarConfiguracoesPIXCD = view.findViewById(R.id.btnSalvarConfiguracoesPIXCD);
        btnCarregarConfiguracoesPIXCD = view.findViewById(R.id.btnCarregarConfiguracoesPIXCD);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        SpinnerUtils.preencherSpinner(getContext(), cmbAmbiente, ambiente);
        SpinnerUtils.preencherSpinner(getContext(), cmbNivelLogPSP, nivelLogPSP);
        SpinnerUtils.preencherSpinner(getContext(), cmbTipoChave, tipoChave);
        SpinnerUtils.preencherSpinner(getContext(), cmbPSP, psp);

        btnSalvarConfiguracoesPIXCD.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesPIXCD();
            }
        });

        btnCarregarConfiguracoesPIXCD.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesPIXCD();
            }
        });

        carregarConfiguracoesPIXCD();

        return view;
    }

    private void salvarConfiguracoesPIXCD(){
        try{
            ACBrPIXCD.configGravarValor("PIXCD", "Ambiente", Integer.toString(cmbAmbiente.getSelectedItemPosition()));
            ACBrPIXCD.configGravarValor("PIXCD", "ArqLog", txtArquivoLogPSP.getText().toString());
            ACBrPIXCD.configGravarValor("PIXCD", "NivelLog", Integer.toString(cmbNivelLogPSP.getSelectedItemPosition()));
            ACBrPIXCD.configGravarValor("PIXCD", "TipoChave", Integer.toString(cmbTipoChave.getSelectedItemPosition()));
            ACBrPIXCD.configGravarValor("PIXCD", "PSP", Integer.toString(cmbPSP.getSelectedItemPosition()));
            ACBrPIXCD.configGravarValor("PIXCD", "Timeout", txtTimeoutPSP.getText().toString());
            ACBrPIXCD.configGravarValor("PIXCD", "ProxyHost", txtProxyServidor.getText().toString());
            ACBrPIXCD.configGravarValor("PIXCD", "ProxyPort", txtProxyPorta.getText().toString());
            ACBrPIXCD.configGravarValor("PIXCD", "ProxyUser", txtProxyUsuario.getText().toString());
            ACBrPIXCD.configGravarValor("PIXCD", "ProxyPass", txtProxySenha.getText().toString());
            ACBrPIXCD.configGravarValor("PIXCD", "NomeRecebedor", txtNomeRecebedor.getText().toString());
            ACBrPIXCD.configGravarValor("PIXCD", "CidadeRecebedor", txtCidadeRecebedor.getText().toString());
            ACBrPIXCD.configGravarValor("PIXCD", "CEPRecebedor", txtCEPRecebedor.getText().toString());
            ACBrPIXCD.configGravarValor("PIXCD", "UFRecebedor", txtUFRecebedor.getText().toString());
            ACBrPIXCD.configGravar();
        } catch (Exception ex){
            Log.i("Erro", " - Salvar Configurações PIXCD: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesPIXCD(){
        try {
            cmbAmbiente.setSelection(Integer.valueOf(ACBrPIXCD.configLerValor("PIXCD", "Ambiente")));
            txtArquivoLogPSP.setText(ACBrPIXCD.configLerValor("PIXCD", "ArqLog"));
            cmbNivelLogPSP.setSelection(Integer.valueOf(ACBrPIXCD.configLerValor("PIXCD", "NivelLog")));
            cmbTipoChave.setSelection(Integer.valueOf(ACBrPIXCD.configLerValor("PIXCD", "TipoChave")));
            cmbPSP.setSelection(Integer.valueOf(ACBrPIXCD.configLerValor("PIXCD", "PSP")));
            txtTimeoutPSP.setText(ACBrPIXCD.configLerValor("PIXCD", "Timeout"));
            txtProxyServidor.setText(ACBrPIXCD.configLerValor("PIXCD", "ProxyHost"));
            txtProxyPorta.setText(ACBrPIXCD.configLerValor("PIXCD", "ProxyPort"));
            txtProxyUsuario.setText(ACBrPIXCD.configLerValor("PIXCD", "ProxyUser"));
            txtProxySenha.setText(ACBrPIXCD.configLerValor("PIXCD", "ProxyPass"));
            txtNomeRecebedor.setText(ACBrPIXCD.configLerValor("PIXCD", "NomeRecebedor"));
            txtCidadeRecebedor.setText(ACBrPIXCD.configLerValor("PIXCD", "CidadeRecebedor"));
            txtCEPRecebedor.setText(ACBrPIXCD.configLerValor("PIXCD", "CEPRecebedor"));
            txtUFRecebedor.setText(ACBrPIXCD.configLerValor("PIXCD", "UFRecebedor"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações PIXCD: " + ex.getMessage());
        }
    }
}