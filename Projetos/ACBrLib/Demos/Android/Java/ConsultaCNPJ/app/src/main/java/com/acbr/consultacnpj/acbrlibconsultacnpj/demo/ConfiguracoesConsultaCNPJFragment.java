package com.acbr.consultacnpj.acbrlibconsultacnpj.demo;

import android.os.Bundle;
import androidx.fragment.app.Fragment;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.AutoCompleteTextView;
import android.widget.Button;
import com.google.android.material.textfield.TextInputEditText;
import br.com.acbr.lib.consultacnpj.ACBrLibConsultaCNPJ;
import br.com.acbr.lib.consultacnpj.Provedor;

public class ConfiguracoesConsultaCNPJFragment extends Fragment {
    private ACBrLibConsultaCNPJ ACBrConsultaCNPJ;

    private AutoCompleteTextView cmbProvedor;
    private TextInputEditText txtUsuarioProvedor;
    private TextInputEditText txtSenhaProvedor;
    private TextInputEditText txtProxyServidor;
    private TextInputEditText txtProxyPorta;
    private TextInputEditText txtProxyUsuario;
    private TextInputEditText txtProxySenha;
    private Button btnSalvarConfiguracoesConsultaCNPJ;
    private Button btnCarregarConfiguracoesConsultaCNPJ;

    private Provedor[] provedor = Provedor.values();

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_consulta_cnpj, container, false);

        ACBrConsultaCNPJ = ACBrLibHelper.getInstance("");

        cmbProvedor = view.findViewById(R.id.cmbProvedor);
        txtUsuarioProvedor = view.findViewById(R.id.txtUsuarioProvedor);
        txtSenhaProvedor = view.findViewById(R.id.txtSenhaProvedor);
        txtProxyServidor = view.findViewById(R.id.txtProxyServidor);
        txtProxyPorta = view.findViewById(R.id.txtProxyPorta);
        txtProxyUsuario = view.findViewById(R.id.txtProxyUsuario);
        txtProxySenha = view.findViewById(R.id.txtProxySenha);

        btnSalvarConfiguracoesConsultaCNPJ = view.findViewById(R.id.btnSalvarConfiguracoesConsultaCNPJ);
        btnCarregarConfiguracoesConsultaCNPJ = view.findViewById(R.id.btnCarregarConfiguracoesConsultaCNPJ);

        // Configurar o adapter para o AutoCompleteTextView
        ArrayAdapter<Provedor> adapter = new ArrayAdapter<>(getContext(), R.layout.dropdown_menu_item, Provedor.values());
        cmbProvedor.setAdapter(adapter);
        cmbProvedor.setThreshold(0);
        cmbProvedor.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                cmbProvedor.showDropDown();
            }
        });

        btnSalvarConfiguracoesConsultaCNPJ.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesConsultaCNPJ();
            }
        });

        btnCarregarConfiguracoesConsultaCNPJ.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesConsultaCNPJ();
            }
        });

        carregarConfiguracoesConsultaCNPJ();

        return view;
    }

    private void salvarConfiguracoesConsultaCNPJ() {
        try {
            String provedorSelecionado = cmbProvedor.getText().toString();
            int posicaoProvedor = -1;
            for (int i = 0; i < provedor.length; i++) {
                if (provedor[i].toString().equals(provedorSelecionado)) {
                    posicaoProvedor = i;
                    break;
                }
            }
            
            ACBrConsultaCNPJ.configGravarValor("ConsultaCNPJ", "Provedor", String.valueOf(posicaoProvedor));
            ACBrConsultaCNPJ.configGravarValor("ConsultaCNPJ", "Usuario", txtUsuarioProvedor.getText().toString());
            ACBrConsultaCNPJ.configGravarValor("ConsultaCNPJ", "Senha", txtSenhaProvedor.getText().toString());
            ACBrConsultaCNPJ.configGravarValor("Proxy", "Servidor", txtProxyServidor.getText().toString());
            ACBrConsultaCNPJ.configGravarValor("Proxy", "Porta", txtProxyPorta.getText().toString());
            ACBrConsultaCNPJ.configGravarValor("Proxy", "Usuario", txtProxyUsuario.getText().toString());
            ACBrConsultaCNPJ.configGravarValor("Proxy", "Senha", txtProxySenha.getText().toString());
            ACBrConsultaCNPJ.configGravar();
        } catch (Exception ex) {
            Log.e("Erro", " - Salvar Configurações Consulta CNPJ: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesConsultaCNPJ() {
        try {
            int posicaoProvedor = Integer.parseInt(ACBrConsultaCNPJ.configLerValor("ConsultaCNPJ", "Provedor"));
            if (posicaoProvedor >= 0 && posicaoProvedor < provedor.length) {
                cmbProvedor.setText(provedor[posicaoProvedor].toString(), false);
            }
            
            txtUsuarioProvedor.setText(ACBrConsultaCNPJ.configLerValor("ConsultaCNPJ", "Usuario"));
            txtSenhaProvedor.setText(ACBrConsultaCNPJ.configLerValor("ConsultaCNPJ", "Senha"));
            txtProxyServidor.setText(ACBrConsultaCNPJ.configLerValor("Proxy", "Servidor"));
            txtProxyPorta.setText(ACBrConsultaCNPJ.configLerValor("Proxy", "Porta"));
            txtProxyUsuario.setText(ACBrConsultaCNPJ.configLerValor("Proxy", "Usuario"));
            txtProxySenha.setText(ACBrConsultaCNPJ.configLerValor("Proxy", "Senha"));
        } catch (Exception ex) {
            Log.e("Erro", " - Carregar Configurações Consulta CNPJ: " + ex.getMessage());
        }
    }
}