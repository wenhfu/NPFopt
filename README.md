## Introduction

`NPFopt.m` is a MATLAB-based **OPT**imization solver that utulizes the **P**enalty-**F**ree method with a **N**onmonotone line search strategy, designed to solve problems with equality or inequality constraints. Its key features include:

- **Nonmonotone Line Search**: Relaxes the requirement for monotonic decrease in the objective function or constraint violation measurement, improving global convergence and Maratos effect avoidance. 
- **Penalty-Free Method**: Avoids numerical instability issues introduced by traditional penalty functions.
- Supports equality constraints (`c(x) = 0`) and inequality constraints (`c(x) ≥ 0`).

The detailed algorithm can be found in the paper  *A penalty-free method with nonmonotone line search for nonlinear optimization* by Ting Xu, Qi Zhao, Wenhao Fu and Zhongwen Chen.

## Installation and Usage

Simply place the `NPFopt.m` file in your MATLAB working directory and call it as needed.

**Basic Syntax**:

```
[x, fval, exitflag, output, lambda] = NPFopt(funf, func, x0);
[x, fval, exitflag, output, lambda] = NPFopt(funf, func, x0, MF);
[x, fval, exitflag, output, lambda] = NPFopt(funf, func, x0, MF, opts);
```

------

## Input Parameters

| Parameter  | Type            | Description                                                  |
| :--------- | :-------------- | :----------------------------------------------------------- |
| `funf`     | Function Handle | Objective function, must return `[fx, gfx]`.<br />Format: `[fx, gfx] = funf(x)`, where `fx` is the objective function value at `x`, `gfx` is the gradient vector at `x`. |
| `func`     | Function Handle | Constraint function, must return `[cx, gcx]`.<br />Format: `[cx, gcx] = func(x)`, where `cx` is the constraint function value at `x`, `gcx` is the Jacobian matrix at `x`. |
| `x0`       | Vector          | Initial point                                                |
| `MF`       | Integer         | Maximum allowed consecutive unsuccessful iterations (default `0`) |
| `opts`     | Struct          | Optional parameters:<br /> `epsilon`: Convergence tolerance (default `1e-5`)<br /> `nfmax`: Maximum function evaluations (default `1000`) <br /> `itermax`: Maximum iterations (default `1000`) <br /> `display`: Level of display (`0` or `1`, default `1`) |

------

## Output Parameters

| Parameter  | Type    | Description                                                  |
| :--------- | :------ | :----------------------------------------------------------- |
| `x`        | Vector  | Optimal solution                                             |
| `fval`     | Scalar  | Objective function value at the solution                     |
| `exitflag` | Integer | Termination flag (see **Exit Flags below**)                  |
| `output`   | Struct  | Detailed information:<br /> `con`: Constraint violation <br />`iter`: Iteration count<br /> `nf`: Objective function evaluations <br />`ngf`: Gradient of objective function evaluations<br /> `nc`: Constraint function evaluations <br />`ngc`: Gradient of constraint function evaluations<br />`n`: Number of variables<br />`m`:Number of constraints<br />`Res`:Optimality error |
| `lambda`   | Vector  | Lagrange multipliers                                         |

------

## Exit Flags

| Value | Description                                             |
| :---- | :------------------------------------------------------ |
| `1`   | Successfully converge to an infeasible stationary point |
| `2`   | Successfully converged to a feasible solution           |
| `0`   | Reached maximum iterations or function evaluations      |
| `-1`  | Line search failed                                      |
| `-2`  | LP subproblem failed                                    |
| `-3`  | QP subproblem failed                                    |
| `-4`  | Numerical problem                                       |
| `-5`  | Problem size too large (dimension ≥ 500)                |

------

## Algorithm Overview

1. **Feasibility Phase**: Generates a feasible direction using a linear programming (LP) subproblem.
2. **Optimality Phase**: Generates an optimal direction using a quadratic programming (QP) subproblem.
3. **Direction Combination**: Combines feasible and optimal directions to get a search direction.
4. **Nonmonotone Line Search**: Relaxes the decrease condition for objective function or constraint violations, dynamically adjusts the step size and updates the upper bound on constraint violations.
5. **BFGS Update**: Approximates the Hessian matrix to accelerate convergence.

------



## Example

### Problem Description

Minimize $f(x)=x_1^2+x_2^2$   , subject to $c(x)=1-x_1-x_2=0$.

### MATLAB Code

```
% Define the objective function and its gradient
funf = @(x) deal(x(1)^2 + x(2)^2, [2*x(1); 2*x(2)]);

% Define the constraint function and its gradient
func = @(x) deal( x(1) - 2, 1 - x(1) - x(2), [1, 0], [-1, -1]);

% Call the solver
x0 = [0; 0];
[x, fval, exitflag, output] = NPFopt(funf, func, x0);

% Display results
disp('Solution:'); disp(x);
disp('Objective function value:'); disp(fval);
```

------

## Notes

1. **Function Definition**: Ensure `funf` and `func` return gradient information; otherwise, the algorithm will fail.
2. **Large-Scale Problems**: The algorithm automatically terminates (exit flag `-5`) if the number of variables or constraints is ≥ 500.
3. **Convergence Tolerance**: Adjust via `opts.epsilon`. Smaller values increase precision but may increase computation time.
4. **Initial Point**: Choose a point close to the feasible region to improve convergence speed.

------



**Author**: Wenhao Fu

**Last Updated**: October 06, 2025
