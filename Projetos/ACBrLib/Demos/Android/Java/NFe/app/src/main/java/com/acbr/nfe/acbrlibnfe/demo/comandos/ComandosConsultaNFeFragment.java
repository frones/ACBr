package com.acbr.nfe.acbrlibnfe.demo.comandos;

import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;

import androidx.fragment.app.Fragment;

import com.acbr.nfe.acbrlibnfe.demo.utils.ACBrLibHelper;
import com.acbr.nfe.acbrlibnfe.demo.R;

import br.com.acbr.lib.nfe.ACBrLibNFe;

public class ComandosConsultaNFeFragment extends Fragment {

    private ACBrLibNFe ACBrNFe;

    private EditText txtRespostaConsulta;
    private EditText txtXMLNFe;
    private EditText txtNumeroRecibo;
    private EditText txtUFConsultarCadastro;
    private EditText txtDocumentoConsultarCadastro;
    private EditText txtConsultarChave;
    private Button btnStatusServico;
    private Button btnConsultarRecibo;
    private Button btnConsultaXml;
    private Button btnConsultarCadastro;
    private Button btnConsultaChave;
    private Button btnLimparRespostaConsulta;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_consulta_nfe, container, false);

        ACBrNFe = ACBrLibHelper.getInstance("");

        txtRespostaConsulta = view.findViewById(R.id.txtRespostaConsulta);
        txtXMLNFe = view.findViewById(R.id.txtXMLNFe);
        txtNumeroRecibo = view.findViewById(R.id.txtNumeroRecibo);
        txtUFConsultarCadastro = view.findViewById(R.id.txtUFConsultarCadastro);
        txtDocumentoConsultarCadastro = view.findViewById(R.id.txtDocumentoConsultarCadastro);
        txtConsultarChave = view.findViewById(R.id.txtConsultarChave);
        btnStatusServico = view.findViewById(R.id.btnStatusServico);
        btnConsultarRecibo = view.findViewById(R.id.btnConsultarRecibo);
        btnConsultaXml = view.findViewById(R.id.btnConsultaXml);
        btnConsultarCadastro = view.findViewById(R.id.btnConsultarCadastro);
        btnConsultaChave = view.findViewById(R.id.btnConsultaChave);
        btnLimparRespostaConsulta = view.findViewById(R.id.btnLimparRespostaConsulta);

        btnStatusServico.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                statusServico();
            }
        });

        btnConsultarRecibo.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                consultarRecibo();
            }
        });

        btnConsultaXml.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                consultarXML();
            }
        });

        btnConsultarCadastro.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                consultarCadastro();
            }
        });

        btnConsultaChave.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                consultarChave();
            }
        });

        btnLimparRespostaConsulta.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaConsulta();
            }
        });

        return view;
    }

    public void statusServico() {
        txtRespostaConsulta.setText("");
        String result = "";
        try {
            result = ACBrNFe.StatusServico();
        } catch (Exception ex) {
            Log.e("Erro ao Consultar Status Serviço", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsulta.setText(result);
        }
    }

    public void consultarRecibo() {
        txtRespostaConsulta.setText("");
        String result = "";
        String numeroRecibo = txtNumeroRecibo.getText().toString();
        try {
            result = ACBrNFe.ConsultarRecibo(numeroRecibo);
        } catch (Exception ex) {
            Log.e("Erro ao Consultar Numero Recibo", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsulta.setText(result);
        }
    }

    public void consultarXML() {
        txtRespostaConsulta.setText("");
        String result = "";
        String xmlNFe = txtXMLNFe.getText().toString();
        try {
            ACBrNFe.LimparLista();
            result = ACBrNFe.Consultar(xmlNFe, false);
        } catch (Exception ex) {
            Log.e("Erro ao Consultar XML", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsulta.setText(result);
        }
    }

    public void consultarCadastro() {
        txtRespostaConsulta.setText("");
        String result = "";
        String UF = txtUFConsultarCadastro.getText().toString();
        String documento = txtDocumentoConsultarCadastro.getText().toString();
        try {
            result = ACBrNFe.ConsultaCadastro(UF, documento, false);
        } catch (Exception ex) {
            Log.e("Erro ao Consultar Cadastro", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsulta.setText(result);
        }
    }

    public void consultarChave() {
        txtRespostaConsulta.setText("");
        String result = "";
        String chave = txtConsultarChave.getText().toString();
        try {
            result = ACBrNFe.Consultar(chave, false);
        } catch (Exception ex) {
            Log.e("Erro ao Consultar Cadastro", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsulta.setText(result);
        }
    }

    public void LimparRespostaConsulta() {
        txtRespostaConsulta.setText("");
    }
}