package com.acbr.nfe.acbrlibnfe.demo;

import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;

import androidx.fragment.app.Fragment;

import br.com.acbr.lib.nfe.ACBrLibNFe;

public class ComandosEventoNFeFragment extends Fragment {

    private ACBrLibNFe ACBrNFe;

    private EditText txtRespostaEvento;
    private EditText txtEventoNFeINI;
    private EditText txtJustificativaCancelarNFe;
    private EditText txtChaveCancelarNFe;
    private EditText txtCNPJCancelarNFe;
    private Button btnCancelarNFe;
    private Button btnCarregarEvento;
    private Button btnEnviarEvento;
    private Button btnLimparListaEvento;
    private Button btnLimparRespostaEvento;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_evento_nfe, container, false);

        ACBrNFe = ACBrLibHelper.getInstance("");

        txtEventoNFeINI = view.findViewById(R.id.txtEventoNFeINI);
        txtRespostaEvento = view.findViewById(R.id.txtRespostaEvento);
        txtJustificativaCancelarNFe = view.findViewById(R.id.txtJustificativaCancelarNFe);
        txtChaveCancelarNFe = view.findViewById(R.id.txtChaveCancelarNFe);
        txtCNPJCancelarNFe = view.findViewById(R.id.txtCNPJCancelarNFe);
        btnCancelarNFe = view.findViewById(R.id.btnCancelarNFe);
        btnCarregarEvento = view.findViewById(R.id.btnCarregarEvento);
        btnEnviarEvento = view.findViewById(R.id.btnEnviarEvento);
        btnLimparListaEvento = view.findViewById(R.id.btnLimparListaEvento);
        btnLimparRespostaEvento = view.findViewById(R.id.btnLimparRespostaEvento);

        btnCancelarNFe.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                cancelarNFe();
            }
        });

        btnCarregarEvento.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarEvento();
            }
        });

        btnEnviarEvento.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                enviarEvento();
            }
        });

        btnLimparListaEvento.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                limparListaEvento();
            }
        });

        btnLimparRespostaEvento.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaEvento();
            }
        });

        return view;
    }

    public void cancelarNFe() {
        txtRespostaEvento.setText("");
        String result = "";
        String justificativa = txtJustificativaCancelarNFe.getText().toString();
        String chave = txtChaveCancelarNFe.getText().toString();
        String cnpj = txtCNPJCancelarNFe.getText().toString();
        try {
            result = ACBrNFe.Cancelar(chave, justificativa, cnpj, 1);
        } catch (Exception ex) {
            Log.e("Erro ao Cancelar NFe", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaEvento.setText(result);
        }
    }

    public void carregarEvento() {
        String EventoNFeINI = txtEventoNFeINI.getText().toString();
        try {
            ACBrNFe.CarregarEventoINI(EventoNFeINI);
        } catch (Exception ex) {
            Log.e("Erro ao Carregar Evento NFe", ex.getMessage());
        }
    }

    public void enviarEvento() {
        txtRespostaEvento.setText("");
        String result = "";
        try {
            result = ACBrNFe.EnviarEvento(1);
        } catch (Exception ex) {
            Log.e("Erro ao Enviar Evento NFe", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaEvento.setText(result);
        }
    }

    public void limparListaEvento() {
        txtRespostaEvento.setText("");
        try {
            ACBrNFe.LimparListaEventos();
            txtRespostaEvento.setText("Método executado com sucesso !!");
        } catch (Exception ex) {
            Log.e("Erro ao Limpar Lista NFe", ex.getMessage());
            txtRespostaEvento.setText(ex.getMessage());
        }
    }

    public void LimparRespostaEvento() {
        txtRespostaEvento.setText("");
    }
}