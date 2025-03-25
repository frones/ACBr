package com.acbr.consultacnpj.acbrlibconsultacnpj.demo;

import android.os.Bundle;
import androidx.fragment.app.Fragment;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import com.google.android.material.textfield.TextInputEditText;
import br.com.acbr.lib.consultacnpj.ACBrLibConsultaCNPJ;

public class ComandosConsultaCNPJFragment extends Fragment {
    private ACBrLibConsultaCNPJ ACBrConsultaCNPJ;

    private TextInputEditText txtConsultarCNPJ;
    private TextInputEditText txtRespostaConsulta;
    private Button btnConsultarCNPJ;
    private Button btnLimparRespostaConsulta;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_consulta_cnpj, container, false);

        ACBrConsultaCNPJ = ACBrLibHelper.getInstance("");

        txtConsultarCNPJ = view.findViewById(R.id.txtConsultarCNPJ);
        txtRespostaConsulta = view.findViewById(R.id.txtRespostaConsulta);
        btnConsultarCNPJ = view.findViewById(R.id.btnConsultarCNPJ);
        btnLimparRespostaConsulta = view.findViewById(R.id.btnLimparRespostaConsulta);

        btnConsultarCNPJ.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                consultarCNPJ();
            }
        });

        btnLimparRespostaConsulta.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                limparRespostaConsultaCNPJ();
            }
        });

        return view;
    }

    private void consultarCNPJ() {
        try {
            String cnpj = txtConsultarCNPJ.getText().toString();
            String resposta = ACBrConsultaCNPJ.consultar(cnpj);
            txtRespostaConsulta.setText(resposta);
        } catch (Exception ex) {
            Log.e("Erro", " - Consultar CNPJ: " + ex.getMessage());
            txtRespostaConsulta.setText(ex.getMessage());
        }
    }

    private void limparRespostaConsultaCNPJ() {
        try {
            txtRespostaConsulta.setText("");
        } catch (Exception ex) {
            Log.e("Erro", " - Limpar Resposta Consulta CNPJ: " + ex.getMessage());
        }
    }
}