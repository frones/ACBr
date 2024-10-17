package com.acbr.nfe.acbrlibnfe.demo.configuracoes;

import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.Spinner;

import androidx.fragment.app.Fragment;

import com.acbr.nfe.acbrlibnfe.demo.utils.ACBrLibHelper;
import com.acbr.nfe.acbrlibnfe.demo.utils.NfeApplication;
import com.acbr.nfe.acbrlibnfe.demo.R;
import com.acbr.nfe.acbrlibnfe.demo.utils.SpinnerUtils;

import br.com.acbr.lib.nfe.ACBrLibNFe;
import br.com.acbr.lib.nfe.ModeloDF;
import br.com.acbr.lib.nfe.TipoEmissao;
import br.com.acbr.lib.nfe.VersaoDF;

public class ConfiguracoesGeraisFragment extends Fragment {

    private ACBrLibNFe ACBrNFe;
    private NfeApplication application;

    private CheckBox ckbAtualizarXML;
    private CheckBox ckbExibirErroSchema;
    private CheckBox ckbRetirarAcentos;
    private CheckBox ckbSalvar;
    private EditText txtFormatoAlerta;
    private EditText txtLogs;
    private EditText txtSchemaPath;
    private EditText txtIdCSC;
    private EditText txtCSC;
    private Spinner cmbFormaEmissao;
    private Spinner cmbModeloDocumento;
    private Spinner cmbVersaoDF;
    private Button btnSalvarConfiguracoesGerais;
    private Button btnCarregarConfiguracoesGerais;

    private TipoEmissao[] tipoEmissao = TipoEmissao.values();
    private ModeloDF[] modeloDF = ModeloDF.values();
    private VersaoDF[] versaoDF = VersaoDF.values();

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_configuracoes_gerais, container, false);

        ACBrNFe = ACBrLibHelper.getInstance("");

        ckbAtualizarXML = view.findViewById(R.id.ckbAtualizarXML);
        ckbExibirErroSchema = view.findViewById(R.id.ckbExibirErroSchemas);
        txtFormatoAlerta = view.findViewById(R.id.txtFormatoAlerta);
        cmbFormaEmissao = view.findViewById(R.id.cmbFormaEmissao);
        cmbModeloDocumento = view.findViewById(R.id.cmbModeloDocumento);
        cmbVersaoDF = view.findViewById(R.id.cmbVersaoDF);
        ckbRetirarAcentos = view.findViewById(R.id.ckbRetirarAcentos);
        ckbSalvar = view.findViewById(R.id.ckbSalvar);
        txtLogs = view.findViewById(R.id.txtLogs);
        txtSchemaPath = view.findViewById(R.id.txtSchemaPath);
        txtIdCSC = view.findViewById(R.id.txtIdCSC);
        txtCSC = view.findViewById(R.id.txtCSC);
        btnSalvarConfiguracoesGerais = view.findViewById(R.id.btnSalvarConfiguracoesGerais);
        btnCarregarConfiguracoesGerais = view.findViewById(R.id.btnCarregarConfiguracoesGerais);
        application = (NfeApplication) this.getContext().getApplicationContext();
        txtSchemaPath.setText(application.getSchemasPath());

        SpinnerUtils.preencherSpinner(getContext(), cmbFormaEmissao, tipoEmissao);
        SpinnerUtils.preencherSpinner(getContext(), cmbModeloDocumento, modeloDF);
        SpinnerUtils.preencherSpinner(getContext(), cmbVersaoDF, versaoDF);

        btnSalvarConfiguracoesGerais.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                salvarConfiguracoesGerais();
            }
        });

        btnCarregarConfiguracoesGerais.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                carregarConfiguracoesGerais();
            }
        });

        carregarConfiguracoesGerais();

        return view;
    }

    private void salvarConfiguracoesGerais() {
        try {
            ACBrNFe.configGravarValor("NFe", "AtualizarXMLCancelado", ckbAtualizarXML.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("NFe", "ExibirErroSchema", ckbExibirErroSchema.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("NFe", "FormatoAlerta", txtFormatoAlerta.getText().toString());
            ACBrNFe.configGravarValor("NFe", "FormaEmissao", Integer.toString(cmbFormaEmissao.getSelectedItemPosition()));
            ACBrNFe.configGravarValor("NFe", "ModeloDF", Integer.toString(cmbModeloDocumento.getSelectedItemPosition()));
            ACBrNFe.configGravarValor("NFe", "VersaoDF", Integer.toString(cmbVersaoDF.getSelectedItemPosition()));
            ACBrNFe.configGravarValor("NFe", "RetirarAcentos", ckbRetirarAcentos.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("NFe", "SalvarWS", ckbSalvar.isChecked() ? Integer.toString(1) : Integer.toString(0));
            ACBrNFe.configGravarValor("NFe", "PathSalvar", txtLogs.getText().toString());
            ACBrNFe.configGravarValor("NFe", "PathSchemas", application.getSchemasPath());
            ACBrNFe.configGravarValor("NFe", "IdCSC", txtIdCSC.getText().toString());
            ACBrNFe.configGravarValor("NFe", "CSC", txtCSC.getText().toString());
            ACBrNFe.configGravarValor("NFE", "PathSalvar", application.getPathSalvar());
            ACBrNFe.configGravar();
        } catch (Exception ex) {
            Log.i("Erro", " - Salvar Configurações Gerais: " + ex.getMessage());
        }
    }

    private void carregarConfiguracoesGerais() {
        try {
            ckbAtualizarXML.setChecked("1".equals(ACBrNFe.configLerValor("NFe", "AtualizarXMLCancelado")));
            ckbExibirErroSchema.setChecked("1".equals(ACBrNFe.configLerValor("NFe", "ExibirErroSchema")));
            txtFormatoAlerta.setText(ACBrNFe.configLerValor("NFe", "FormatoAlerta"));
            cmbFormaEmissao.setSelection(Integer.valueOf(ACBrNFe.configLerValor("NFe", "FormaEmissao")));
            cmbModeloDocumento.setSelection(Integer.valueOf(ACBrNFe.configLerValor("NFe", "ModeloDF")));
            cmbVersaoDF.setSelection(Integer.valueOf(ACBrNFe.configLerValor("NFe", "VersaoDF")));
            ckbRetirarAcentos.setChecked("1".equals(ACBrNFe.configLerValor("NFe", "RetirarAcentos")));
            ckbSalvar.setChecked("1".equals(ACBrNFe.configLerValor("NFe", "SalvarWS")));
            txtLogs.setText(ACBrNFe.configLerValor("NFe", "PathSalvar"));
            txtSchemaPath.setText(ACBrNFe.configLerValor("NFe", "PathSchemas"));
            txtIdCSC.setText(ACBrNFe.configLerValor("NFe", "IdCSC"));
            txtCSC.setText(ACBrNFe.configLerValor("NFe", "CSC"));
        } catch (Exception ex) {
            Log.i("Erro", " - Carregar Configurações Gerais: " + ex.getMessage());
        }
    }
}