package com.acbr.pixcd.acbrlibpixcd.demo.comandos.endpoints.cob;

import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.Spinner;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ACBrLibHelper;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.SpinnerUtils;

import java.text.SimpleDateFormat;
import java.util.Date;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ComandosConsultarCobrancasCobFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txtDataInicialConsultarCobrancasCob;
    private EditText txtDataFinalConsultarCobrancasCob;
    private EditText txtCPFCNPJConsultarCobrancasCob;
    private Spinner cmbStatusConsultarCobrancasCob;
    private CheckBox ckbLocationConsultarCobrancasCob;
    private EditText txtPagAtualConsultarCobrancasCob;
    private EditText txtItensPorPaginaConsultarCobrancasCob;
    private Button btnConsultarCobrancasCob;
    private EditText txtRespostaConsultarCobrancasCob;
    private Button btnLimparRespostaConsultarCobrancasCob;

    private String[] status = {"Nenhum", "Ativa", "Concluída", "Removida Pelo Usuário Recebedor", "Removida Pelo PSP"};

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_consultar_cobrancas_cob, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txtDataInicialConsultarCobrancasCob = view.findViewById(R.id.txtDataInicialConsultarCobrancasCob);
        txtDataFinalConsultarCobrancasCob = view.findViewById(R.id.txtDataFinalConsultarCobrancasCob);
        txtCPFCNPJConsultarCobrancasCob = view.findViewById(R.id.txtCPFCNPJConsultarCobrancasCob);
        cmbStatusConsultarCobrancasCob = view.findViewById(R.id.cmbStatusConsultarCobrancasCob);
        ckbLocationConsultarCobrancasCob = view.findViewById(R.id.ckbLocationConsultarCobrancasCob);
        txtPagAtualConsultarCobrancasCob = view.findViewById(R.id.txtPagAtualConsultarCobrancasCob);
        txtItensPorPaginaConsultarCobrancasCob = view.findViewById(R.id.txtItensPorPaginaConsultarCobrancasCob);
        btnConsultarCobrancasCob = view.findViewById(R.id.btnConsultarCobrancasCob);
        txtRespostaConsultarCobrancasCob = view.findViewById(R.id.txtRespostaConsultarCobrancasCob);
        btnLimparRespostaConsultarCobrancasCob = view.findViewById(R.id.btnLimparRespostaConsultarCobrancasCob);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        SpinnerUtils.preencherSpinner(getContext(), cmbStatusConsultarCobrancasCob, status);

        btnConsultarCobrancasCob.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                ConsultarCobrancasCob();
            }
        });

        btnLimparRespostaConsultarCobrancasCob.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaConsultarCobrancasCob();
            }
        });

        return view;
    }

    public void ConsultarCobrancasCob(){
        SimpleDateFormat sdf = new SimpleDateFormat("dd/MM/yyyy");
        txtRespostaConsultarCobrancasCob.setText("");
        String result = "";
        String dataInicial = txtDataInicialConsultarCobrancasCob.getText().toString();
        String dataFinal = txtDataFinalConsultarCobrancasCob.getText().toString();
        String cpfCnpj = txtCPFCNPJConsultarCobrancasCob.getText().toString();
        int pagAtual = Integer.parseInt(txtPagAtualConsultarCobrancasCob.getText().toString());
        int itensPorPagina = Integer.parseInt(txtItensPorPaginaConsultarCobrancasCob.getText().toString());
        try{
            Date dateInicial = sdf.parse(dataInicial);
            Date dateFinal = sdf.parse(dataFinal);
            double tDateTimeInicial = ACBrPIXCD.convertDateToTDateTime(dateInicial);
            double tDateTimeFinal = ACBrPIXCD.convertDateToTDateTime(dateFinal);
            result = ACBrPIXCD.ConsultarCobrancasCob(tDateTimeInicial, tDateTimeFinal, cpfCnpj, ckbLocationConsultarCobrancasCob.isChecked(), cmbStatusConsultarCobrancasCob.getSelectedItemPosition(), pagAtual, itensPorPagina);
        } catch (Exception ex){
            Log.e("Erro ao Consultar Cobranças Cob", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsultarCobrancasCob.setText(result);
        }

    }

    public void LimparRespostaConsultarCobrancasCob(){
        txtRespostaConsultarCobrancasCob.setText("");
    }
}