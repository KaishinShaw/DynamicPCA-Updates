# DynamicPCA-Updates
This repository contains the course project for the R language class during the Spring 2024 semester at Southern Medical University. 

## Project Description

This repository contains the course project for the R language class during the Spring 2024 semester at Southern Medical University. The project focuses on implementing two fast algorithms for dynamic PCA (Principal Component Analysis) updates: recursive and stochastic gradient methods. The codebase also includes auxiliary functions designed to update the sample mean and covariance.

### Key Features

- **Recursive PCA Update Algorithm**: Efficiently updates principal components as new data arrives, using a recursive methodology.
- **Stochastic Gradient PCA Update Algorithm**: Implements a more randomized approach to updating principal components, which can be advantageous in large-scale applications.
- **Utility Functions**: Functions to update the sample mean and sample covariance, essential for maintaining accurate PCA in dynamic environments.

### Current Challenges

- **Convergence Issue in `secular_pca` Solver**: We have identified that the `secular_pca` solver does not always converge within a finite number of iterations under certain conditions. This issue is currently under investigation, and further research and debugging are needed to resolve this problem.

### Goals for Improvement

- **Enhance Solver Stability**: Investigate and enhance the stability and reliability of the `secular_pca` solver.
- **Optimize Performance**: Continuous optimization of the algorithms to handle larger datasets more efficiently.
- **Expand Documentation**: Improve documentation to provide clear usage instructions and examples.

### License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

---

This repository is an academic project and is part of the ongoing learning and research in dynamic PCA methods at Southern Medical University. Contributions that help improve the functionality and accuracy of these PCA update algorithms are highly appreciated.