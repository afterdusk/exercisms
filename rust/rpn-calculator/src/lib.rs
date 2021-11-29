#[derive(Debug)]
pub enum CalculatorInput {
    Add,
    Subtract,
    Multiply,
    Divide,
    Value(i32),
}

pub fn evaluate(inputs: &[CalculatorInput]) -> Option<i32> {
    let mut stack: Vec<i32> = vec![];
    for input in inputs {
        match input {
            CalculatorInput::Value(v) => stack.push(*v),
            _ => {
                if stack.len() < 2 { return None };
                let operand_2 = stack.pop().unwrap_or_default();
                let operand_1 = stack.pop().unwrap_or_default();
                let result = match input {
                    CalculatorInput::Add => operand_1 + operand_2,
                    CalculatorInput::Subtract => operand_1 - operand_2,
                    CalculatorInput::Multiply => operand_1 * operand_2,
                    CalculatorInput::Divide => operand_1 / operand_2,
                    _ => panic!("operation inserted into stack")
                };
                stack.push(result)
            }
        }
    }
    stack.pop().filter(|_| {stack.is_empty()})
}
